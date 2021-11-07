module Semaphore.Exhaustive exposing (..)

{-| Like the base Semaphore module, except all paths through the programs are traced, with each input ConcurrentProgram emitting a list of ConcurrentProgram outputs.
-}

import Dict
import Internal.Types exposing (..)
import Internal.Utils as Utils
import List.Extra as LE
import Semaphore



--------------------------------------------------------------------------------
-- Constructor


init :
    b
    -> List (Thread a b)
    -> List ( String, Semaphore )
    -> ConcurrentProgram a b
init =
    Semaphore.init



--------------------------------------------------------------------------------
-- Exhaustive Executin


run : ConcurrentProgram a b -> List (ConcurrentProgram a b)
run program =
    case program of
        Running p ->
            advance p |> List.concatMap run

        Deadlock outputs ->
            [ List.map List.reverse outputs
                |> Deadlock
            ]

        Completed ( sharedState, outputs ) ->
            [ ( sharedState, List.map List.reverse outputs )
                |> Completed
            ]

        Invalid outputs ->
            [ List.map List.reverse outputs
                |> Invalid
            ]


advance : ActiveProgram a b -> List (ConcurrentProgram a b)
advance p =
    case p.activeThreads of
        [] ->
            case List.isEmpty p.blockedThreads of
                False ->
                    [ Deadlock <| List.map Tuple.second p.blockedThreads ]

                True ->
                    [ Completed ( p.sharedState, p.outputs ) ]

        xs ->
            Utils.listPairs xs
                |> List.concatMap
                    (\( y, ys ) ->
                        execThread y { p | activeThreads = ys }
                    )


execThread :
    ThreadPair a b
    -> ActiveProgram a b
    -> List (ConcurrentProgram a b)
execThread ( ( stmts, threadState ), output ) p =
    case stmts of
        [] ->
            [ Running { p | outputs = output :: p.outputs } ]

        x :: xs ->
            execInstruction x xs threadState output p


execInstruction :
    Statement a b
    -> List (Statement a b)
    -> a
    -> Output a b
    -> ActiveProgram a b
    -> List (ConcurrentProgram a b)
execInstruction stmt stmts threadState output p =
    case stmt of
        Expression f ->
            [ Semaphore.execExpression f stmt stmts threadState output p ]

        Signal semName ->
            execSignal semName stmt stmts threadState output p

        Wait semName ->
            [ Semaphore.execWait semName stmt stmts threadState output p ]


execSignal :
    String
    -> Statement a b
    -> List (Statement a b)
    -> a
    -> Output a b
    -> ActiveProgram a b
    -> List (ConcurrentProgram a b)
execSignal semName stmt stmts threadState output p =
    let
        ( semaphoreDict, value ) =
            Semaphore.tryUpdate ((+) 1) semName p.semaphores
    in
    case value of
        Nothing ->
            [ (InvalidSemaphore semName :: output)
                :: p.outputs
                |> Invalid
            ]

        Just n ->
            let
                threadPair =
                    ( ( stmts, threadState )
                    , Signaled semName n :: output
                    )
            in
            execSignalHelper threadPair semaphoreDict p


execSignalHelper :
    ThreadPair a b
    -> Dict.Dict String Semaphore
    -> ActiveProgram a b
    -> List (ConcurrentProgram a b)
execSignalHelper threadPair semaphoreDict p =
    let
        f unblocked blocked =
            let
                unblocked_ =
                    Tuple.mapSecond ((::) Unblocked) unblocked
            in
            Running
                { p
                    | activeThreads =
                        p.activeThreads ++ [ unblocked_, threadPair ]
                    , blockedThreads = blocked
                    , semaphores = semaphoreDict
                }
    in
    case p.blockedThreads of
        [] ->
            [ Running
                { p
                    | activeThreads = p.activeThreads ++ [ threadPair ]
                    , semaphores = semaphoreDict
                }
            ]

        xs ->
            Utils.listPairs xs
                |> List.map (\( y, ys ) -> f y ys)



--------------------------------------------------------------------------------
-- Analysis


summarize : List (ConcurrentProgram a b) -> ExecutionSummary b
summarize programs =
    let
        ( completedPrograms, deadlockedPrograms, invalidPrograms ) =
            Utils.groupResults programs
    in
    { totalCount = List.length programs
    , totalUniqueCount = LE.unique programs |> List.length
    , completed = summarizeCompleted completedPrograms
    , deadlocked = summarizeResults deadlockedPrograms
    , invalid = summarizeResults invalidPrograms
    }


summarizeCompleted : List ( b, List (Output a b) ) -> ResultsSummary b
summarizeCompleted pairs =
    { count = List.length pairs
    , uniqueCount = LE.unique pairs |> List.length
    , sharedStates = List.map Tuple.first pairs |> LE.unique |> Just
    , outputStats = List.map Tuple.second pairs |> summarizeOutputs
    }


summarizeResults : List (List (Output a b)) -> ResultsSummary b
summarizeResults outputs =
    { count = List.length outputs
    , uniqueCount = LE.unique outputs |> List.length
    , sharedStates = Nothing
    , outputStats = summarizeOutputs outputs
    }


summarizeOutputs : List (List (Output a b)) -> OutputStats
summarizeOutputs outputs =
    { avgThreadLength =
        List.map Utils.avgLength outputs
            |> Utils.intAvg
    , minThreadLength =
        List.map Utils.minLength outputs
            |> List.minimum
            |> Maybe.withDefault 0
    , maxThreadLength =
        List.map Utils.maxLength outputs
            |> List.maximum
            |> Maybe.withDefault 0
    }
