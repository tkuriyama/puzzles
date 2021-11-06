module Semaphore exposing (..)

import Dict



--------------------------------------------------------------------------------


type ConcurrentProgram a b
    = Running (ActiveProgram a b)
    | Deadlock (List (Output a b))
    | Completed ( b, List (Output a b) )
    | Invalid (List (Output a b))


type alias ActiveProgram a b =
    { sharedState : b
    , activeThreads : List (ThreadPair a b)
    , blockedThreads : List (ThreadPair a b)
    , outputs : List (Output a b)
    , semaphores : Dict.Dict String Semaphore
    }


type alias Semaphore =
    Int


type alias ThreadPair a b =
    ( Thread a b, Output a b )


type alias Thread a b =
    ( List (Statement a b), a )


type Statement a b
    = Expression (( a, b ) -> ( a, b ))
    | Signal String
    | Wait String


type alias Output a b =
    List (OutputMsg a b)


type OutputMsg a b
    = Evaluated ( a, b )
    | Signaled String Semaphore
    | NoWait String Semaphore
    | Waiting String Semaphore
    | InvalidSemaphore String
    | Unblocked



--------------------------------------------------------------------------------


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


run : ConcurrentProgram a b -> ConcurrentProgram a b
run program =
    case program of
        Running p ->
            advance p |> run

        Deadlock outputs ->
            List.map List.reverse outputs
                |> Deadlock

        Completed ( sharedState, outputs ) ->
            ( sharedState, List.map List.reverse outputs )
                |> Completed

        Invalid outputs ->
            List.map List.reverse outputs
                |> Invalid


advance : ActiveProgram a b -> ConcurrentProgram a b
advance p =
    case p.activeThreads of
        [] ->
            case List.isEmpty p.blockedThreads of
                False ->
                    Deadlock <| List.map Tuple.second p.blockedThreads

                True ->
                    Completed ( p.sharedState, p.outputs )

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

        Signal semName ->
            execSignal semName stmt stmts threadState output p

        Wait semName ->
            execWait semName stmt stmts threadState output p


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
            (InvalidSemaphore semName :: output)
                :: p.outputs
                |> Invalid

        Just n ->
            let
                threadPair =
                    ( ( stmts, threadState )
                    , Signaled semName n :: output
                    )
            in
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
            (InvalidSemaphore semName :: output)
                :: p.outputs
                |> Invalid

        Just n ->
            case n < 0 of
                True ->
                    let
                        threadPair =
                            ( thread
                            , Waiting semName 0 :: output
                            )
                    in
                    Running
                        { p | blockedThreads = p.blockedThreads ++ [ threadPair ] }

                False ->
                    let
                        threadPair =
                            ( thread
                            , NoWait semName n :: output
                            )
                    in
                    Running
                        { p
                            | activeThreads = p.activeThreads ++ [ threadPair ]
                            , semaphores = semaphoreDict
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
