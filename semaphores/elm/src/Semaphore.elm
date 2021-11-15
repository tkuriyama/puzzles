module Semaphore exposing (..)

{-| Model the execution of simple concurrenc threads with semaphores.

The order of execution is based on the hard-coded logic (no randomization).
So, while no correct program should result, in say, a deadlock, the fact
that a program completes as expected doesn't guarantee it's correct.

See Semaphores.Exhaustive for modeling all execution paths.

-}

import Dict
import Internal.Types exposing (..)



--------------------------------------------------------------------------------
-- Constructor


init :
    b
    -> List (Thread a b)
    -> List ( String, Semaphore )
    -> ConcurrentProgram a b
init sharedState threads semaphorePairs =
    Running
        { sharedState = sharedState
        , activeThreads = List.map (\t -> ( t, [] )) threads
        , blockedThreads = []
        , outputs = []
        , semaphores = Dict.fromList semaphorePairs
        }



--------------------------------------------------------------------------------
-- Execute


run : ConcurrentProgram a b -> ConcurrentProgram a b
run program =
    case program of
        Running p ->
            advance p |> run

        Deadlock ( semaphores, outputs ) ->
            ( semaphores, List.map List.reverse outputs )
                |> Deadlock

        Completed ( sharedState, semaphores, outputs ) ->
            ( sharedState, semaphores, List.map List.reverse outputs )
                |> Completed

        Invalid ( semaphores, outputs ) ->
            ( semaphores, List.map List.reverse outputs )
                |> Invalid


advance : ActiveProgram a b -> ConcurrentProgram a b
advance p =
    case p.activeThreads of
        [] ->
            case List.isEmpty p.blockedThreads of
                False ->
                    Deadlock
                        ( p.semaphores, List.map Tuple.second p.blockedThreads )

                True ->
                    Completed
                        ( p.sharedState, p.semaphores, p.outputs )

        x :: xs ->
            execThread x { p | activeThreads = xs }


execThread :
    ThreadPair a b
    -> ActiveProgram a b
    -> ConcurrentProgram a b
execThread ( ( stmts, threadState ), output ) p =
    case stmts of
        [] ->
            Running { p | outputs = output :: p.outputs }

        x :: xs ->
            execInstruction x xs threadState output p


execInstruction :
    Statement a b
    -> List (Statement a b)
    -> a
    -> Output a b
    -> ActiveProgram a b
    -> ConcurrentProgram a b
execInstruction stmt stmts threadState output p =
    case stmt of
        Expression f ->
            execExpression f stmt stmts threadState output p

        SemaphoreStatement f ->
            case f ( threadState, p.sharedState ) of
                Signal semName ->
                    execSignal semName stmt stmts threadState output p

                Wait semName ->
                    execWait semName stmt stmts threadState output p

                Pass ->
                    execPass stmts threadState output p


execExpression :
    (( a, b ) -> ( a, b ))
    -> Statement a b
    -> List (Statement a b)
    -> a
    -> Output a b
    -> ActiveProgram a b
    -> ConcurrentProgram a b
execExpression f stmt stmts threadState output p =
    let
        ( threadState_, sharedState_ ) =
            f ( threadState, p.sharedState )

        threadPair =
            ( ( stmts, threadState_ )
            , Evaluated ( threadState_, sharedState_ ) :: output
            )
    in
    Running
        { p
            | sharedState = sharedState_
            , activeThreads = p.activeThreads ++ [ threadPair ]
        }


execSignal :
    String
    -> Statement a b
    -> List (Statement a b)
    -> a
    -> Output a b
    -> ActiveProgram a b
    -> ConcurrentProgram a b
execSignal semName stmt stmts threadState output p =
    let
        ( semaphoreDict, value ) =
            tryUpdate ((+) 1) semName p.semaphores
    in
    case value of
        Nothing ->
            ( p.semaphores
            , (InvalidSemaphore semName :: output) :: p.outputs
            )
                |> Invalid

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
    -> SemaphoreDict
    -> ActiveProgram a b
    -> ConcurrentProgram a b
execSignalHelper threadPair semaphoreDict p =
    case p.blockedThreads of
        [] ->
            Running
                { p
                    | activeThreads = p.activeThreads ++ [ threadPair ]
                    , semaphores = semaphoreDict
                }

        x :: xs ->
            let
                unblocked =
                    Tuple.mapSecond ((::) Unblocked) x
            in
            Running
                { p
                    | activeThreads =
                        p.activeThreads
                            ++ [ unblocked, threadPair ]
                    , blockedThreads = xs
                    , semaphores = semaphoreDict
                }


execWait :
    String
    -> Statement a b
    -> List (Statement a b)
    -> a
    -> Output a b
    -> ActiveProgram a b
    -> ConcurrentProgram a b
execWait semName stmt stmts threadState output p =
    let
        ( semaphoreDict, value ) =
            tryUpdate ((+) -1) semName p.semaphores

        thread =
            ( stmts, threadState )
    in
    case value of
        Nothing ->
            ( p.semaphores
            , (InvalidSemaphore semName :: output) :: p.outputs
            )
                |> Invalid

        Just n ->
            execWaitHelper thread output n semName semaphoreDict p


execWaitHelper :
    Thread a b
    -> Output a b
    -> Int
    -> String
    -> SemaphoreDict
    -> ActiveProgram a b
    -> ConcurrentProgram a b
execWaitHelper newThread output n semName semaphoreDict p =
    case n < 0 of
        True ->
            let
                threadPair =
                    ( newThread
                    , Waiting semName n :: output
                    )
            in
            Running
                { p
                    | blockedThreads = p.blockedThreads ++ [ threadPair ]
                    , semaphores = semaphoreDict
                }

        False ->
            let
                threadPair =
                    ( newThread
                    , NoWait semName n :: output
                    )
            in
            Running
                { p
                    | activeThreads = p.activeThreads ++ [ threadPair ]
                    , semaphores = semaphoreDict
                }


execPass :
    List (Statement a b)
    -> a
    -> Output a b
    -> ActiveProgram a b
    -> ConcurrentProgram a b
execPass stmts threadState output p =
    let
        threadPair =
            ( ( stmts, threadState )
            , NoAction :: output
            )
    in
    Running
        { p
            | activeThreads = p.activeThreads ++ [ threadPair ]
        }



--------------------------------------------------------------------------------


tryUpdate :
    (val -> val)
    -> comparable
    -> Dict.Dict comparable val
    -> ( Dict.Dict comparable val, Maybe val )
tryUpdate f key dict =
    case Dict.get key dict of
        Nothing ->
            ( dict, Nothing )

        Just val ->
            ( Dict.update key (Maybe.map f) dict
            , Just <| f val
            )
