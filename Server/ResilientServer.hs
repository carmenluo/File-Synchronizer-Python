{-# LANGUAGE OverloadedStrings #-}
module ResilientServer where


import           Control.Concurrent             (forkIO)
import           Control.Concurrent.Async       (race)
import           Control.Concurrent.STM         (TChan, atomically, readTChan, newTChan,
                                                 writeTChan, STM)
import           Control.Monad                  (forever)
import           Control.Monad.IO.Class         (liftIO)
import           Data.Text.IO                   as Tio

import           Network.HTTP.Types             (status400)
import           Network.Wai                    (Application, responseLBS)
import           Network.Wai.Handler.Warp       (run)
import           Network.Wai.Handler.WebSockets (websocketsOr)
import qualified Data.Map.Strict                as M'
import qualified Network.WebSockets             as WS
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import qualified Data.Text.Read                 as T
import           Data.IntMap.Strict             (IntMap)
import qualified Data.IntMap.Strict             as IM


-- We may need to change event text to just string or text (consult anand)
-- NewUser is an implicitly defined wrapper

data CentralMessage
    = SLogin (TChan ClientMessage) Text Text  -- name password, will get a response of LoginAccepted and channel will be recorded, or LoginRejected
    | Logout UserId
    | RegisterUser (TChan ClientMessage) Text Text                  -- register new name and password
    -- for now, we will not decode user activities, just store them as strings
    -- but we do need to look them up by day, so we will store that, and can send back the TimeStamps to confirm we have stored those activities
    | NewActivity (TChan ClientMessage) UserId DayId TimeStamp Text  -- Ack reply
    -- request activities to display in Calendar
    | RequestData (TChan ClientMessage) UserId DayId


data ClientMessage
    = UserAccepted UserId          -- sent on login or restart
    | SupervisorAccepted UserId    -- sent on login or restart
    | SuperUserAccepted UserId
    | LoginRejected
    
    -- when a new user is added, send acknowledgment with user name
    | NewUserOk Text 
    | DuplicateUser Text 
    -- acknowledge saving of data, the UserId is to make sure we are sending to the right user
    | AckActivity UserId TimeStamp
    | ErrActivity UserId TimeStamp Text  -- error message
    -- sending back data
    | Retrieve UserId DayId [(TimeStamp,Text)]
    | ErrRetrieve UserId DayId Text -- error message
    

data UserType = User | Supervisor | SuperUser
    deriving (Show,Eq,Ord)

type TimeStamp = Int
type DayId = Int
type UserId = Int

-- what phase are we in?  phases listed in order.  When finished asking questions, we go back to WaitForStart
data GlobalPhase = WaitForStart | GetQs | RankQs | AskQs

data CentralState = CentralState
    { clients          :: IntMap (Text,Text,UserType)        -- UserId -> name, password, usertype
    , name2PassId      :: M'.Map Text (Text,UserId)          -- name -> password, UserId
    , connected        :: IntMap (TChan ClientMessage)
    , activities       :: IntMap (IntMap (IntMap Text))      -- UserId -> DayId -> TimeStamp -> Text
    , nextUserId       :: UserId
    }

wsApp :: TChan CentralMessage -> WS.ServerApp
wsApp centralMessageChan pendingConn = do
    -- This function handles each new Connection.
    conn <- WS.acceptRequest pendingConn
    WS.forkPingThread conn 1

    WS.sendTextData conn ("w:0.1" :: Text)

    -- atomically means that the thread clientMessageChan cannot be viewed and it cannot access anything that has been modified by
    -- other threads. Once it exits the thread then everything becomes visible.
    -- However if the modifictions this thread is making are not being modifed by any other threads, the change becomes visible.
    -- If two atomically threads are modifying the same variable then the first one to exit will commit the changes, the other one will
    -- fail and retry.

    -- Get a new message channel for this client.
    clientMessageChan <- atomically (newTChan :: STM (TChan ClientMessage))
    
    -- wait for login message, so we can run different client handlers for different types of users
    rawMsg <- WS.receiveData conn
    Tio.putStrLn $ T.concat ["Got message:", rawMsg]
    
    case T.splitOn ":" rawMsg of
        "s":u:p:[] -> do
            atomically $ writeTChan centralMessageChan (SLogin clientMessageChan u p)
            message <- atomically $ readTChan clientMessageChan

            case message of
                UserAccepted userId              -> 
                    do
                        let msgTxt = T.concat            ["u:", tShow userId] 
                        Tio.putStrLn $ T.concat ["Sending: ", msgTxt]
                        WS.sendTextData conn msgTxt
                        -- Run two threads and terminate when either dies.
                        -- (1) Process the new message channel responsible for this Client.
                        -- (2) Read the WebSocket connection and pass it on to the central
                        --     message channel.
                        race (processClientChan conn clientMessageChan)
                             (processUserRequests conn userId clientMessageChan centralMessageChan)

                        return ()  -- We don't care about the results of `race`
        
                SupervisorAccepted userId        -> 
                    do
                        let msgTxt = T.concat            ["s:", tShow userId] 
                        Tio.putStrLn $ T.concat ["Sending: ", msgTxt]
                        WS.sendTextData conn msgTxt
                        race (processClientChan conn clientMessageChan)
                             (processSupervisorRequests conn userId clientMessageChan centralMessageChan)

                        return ()  -- We don't care about the results of `race`
        
                SuperUserAccepted userId         -> 
                    do
                        let msgTxt = T.concat            ["su:", tShow userId] 
                        Tio.putStrLn $ T.concat ["Sending: ", msgTxt]
                        WS.sendTextData conn msgTxt
                        race (processClientChan conn clientMessageChan)
                             (processSuperUserRequests conn userId clientMessageChan centralMessageChan)

                        return ()  -- We don't care about the results of `race`
        _ -> do
            Tio.putStrLn $ T.concat ["expected signon, got ",rawMsg]
        
processUserRequests conn userId clientChan centralChan = forever $ do
        msg <- WS.receiveData conn
        Tio.putStrLn $ T.concat ["Got message:", msg]
        case T.splitOn ":" msg of
            "x":[] -> do 
                  Tio.putStrLn "logging out"
                  atomically $ writeTChan centralChan $ Logout userId
                  return ()
            "n":uIdTxt:dayIdTxt:timeStampTxt:txt:[] -> 
                case map T.decimal [uIdTxt,dayIdTxt,timeStampTxt] of
                    [Right (uid,_), Right (did,_), Right (ts,_)]
                        -> atomically $ writeTChan centralChan $ NewActivity clientChan uid did ts txt 
                    err -> Tio.putStrLn $ T.concat ["Parse Error (n): ",tShow err]
            "d":uIdTxt:dayIdTxt:[] -> 
                case (T.decimal uIdTxt, T.decimal dayIdTxt) of
                    (Right (uId,_),Right (dayId,_))
                                           -> atomically $ writeTChan centralChan $ RequestData clientChan uId dayId
                    partial      -> Tio.putStrLn $ T.concat ["Parse Error (d): ",tShow partial]
            _              -> Tio.putStrLn $ T.concat ["Parse or Permission Error (): ",msg]

processSupervisorRequests conn userId clientChan centralChan = forever $ do
        msg <- WS.receiveData conn
        Tio.putStrLn $ T.concat ["Got message:", msg]
        case T.splitOn ":" msg of
            "x":[] -> do 
                Tio.putStrLn "logging out"
                atomically $ writeTChan centralChan $ Logout userId
                return ()
            "r":u:p:[] -> atomically $ writeTChan centralChan $ RegisterUser clientChan u p
            _              -> Tio.putStrLn $ T.concat ["Parse or Permission Error (): ",msg]

processSuperUserRequests conn userId clientChan centralChan = forever $ do
        msg <- WS.receiveData conn
        Tio.putStrLn $ T.concat ["Got message:", msg]
        case T.splitOn ":" msg of
            "x":[] -> do 
                Tio.putStrLn "logging out"
                atomically $ writeTChan centralChan $ Logout userId
                return ()
            _              -> Tio.putStrLn $ T.concat ["Parse or Permission Error (): ",msg]


--  put message encoding here, so it is easy to match on the client side
--  *** could add level of safety by having different functions for different classes of users
processClientChan :: WS.Connection -> TChan ClientMessage -> IO ()
processClientChan conn chan = forever $ do
    -- This reads a ClientMessage channel forever and passes any messages
    -- it reads to the WebSocket Connection.
    message <- atomically $ readTChan chan
    let msgTxt = case message of
                   LoginRejected                    ->                      "x"
                   SupervisorAccepted userId        -> T.concat            ["s:", tShow userId]       
                   SuperUserAccepted userId         -> T.concat            ["su:", tShow userId]       
                   UserAccepted userId              -> T.concat            ["u:", tShow userId]       

                   NewUserOk name                   -> T.concat            ["r:", name]       
                   DuplicateUser name               -> T.concat            ["xr:", name]       
                   AckActivity userId ts            -> T.intercalate ":" $ ["n", tShow userId, tShow ts] 
                   ErrActivity userId ts err        -> T.intercalate ":" $ ["N", tShow userId, tShow ts, err]
                   Retrieve userId dayId dayTSs     -> T.intercalate ":" $  "d": tShow userId: tShow dayId: (concatMap (\(d,t) -> [tShow d,tShow t]) dayTSs)
                   ErrRetrieve userId dayId txt     -> T.intercalate ":" $ ["D", tShow userId, tShow dayId, txt]
    Tio.putStrLn $ T.concat ["Sending: ", msgTxt]
    WS.sendTextData conn msgTxt


processCentralMessage :: CentralState -> CentralMessage -> IO CentralState
processCentralMessage centralState msg =
    case msg of
        SLogin chan name password ->
            -- A new Client is trying to log on.
            -- if their name and password are ok, add the channel to the list of connected channels, and send accepted message
            -- otherwise send rejected message

            case M'.lookup name (name2PassId centralState) of
                Nothing -> do
                    atomically $ writeTChan chan LoginRejected
                    return centralState                     -- nothing changed
                Just (pw,id) ->
                    if pw == password 
                        then
                            case IM.lookup id (clients centralState) of
                                Just (_,_,User) -> do
                                                          atomically $ writeTChan chan (UserAccepted id)
                                                          return $ centralState { connected = IM.insert id chan (connected centralState) }
                                Just (_,_,Supervisor)-> do
                                                          atomically $ writeTChan chan (SupervisorAccepted id)
                                                          return $ centralState { connected = IM.insert id chan (connected centralState) }
                                Just (_,_,SuperUser) -> do
                                                          atomically $ writeTChan chan (SuperUserAccepted id)
                                                          return $ centralState { connected = IM.insert id chan (connected centralState) }
                                x                  -> do
                                                          Tio.putStrLn $ T.concat ["client lookup failure ",tShow x,tShow id,name]
                                                          atomically $ writeTChan chan LoginRejected
                                                          return centralState
                        else do
                            atomically $ writeTChan chan LoginRejected
                            return centralState                     -- nothing changed

        Logout userId ->
            case IM.lookup userId (connected centralState) of
                Nothing -> do
                    Tio.putStrLn $ T.concat ["tried to log out user who doesn't exist ", tShow userId]
                    return centralState
                Just _ -> do
                    Tio.putStrLn $ T.concat ["log out ", tShow userId]
                    return $ centralState { connected = IM.delete userId (connected centralState) }
       
        RegisterUser chan userName password -> do
            let nextId = nextUserId centralState
            case M'.lookup userName (name2PassId centralState) of
                Just _ -> do
                    Tio.putStrLn $ T.concat ["tried to register duplicate name", userName]
                    atomically $ writeTChan chan $ DuplicateUser userName
                    return centralState
                Nothing -> do
                    let newState = centralState { clients = IM.insert nextId (userName, password, User) (clients centralState)
                                                , name2PassId = M'.insert userName (password,nextId) (name2PassId centralState)
                                                , nextUserId = 1 + nextId
                                                }
                    atomically $ writeTChan chan $ NewUserOk userName
                    return newState

processCentralChan :: TChan CentralMessage -> IO ()
processCentralChan chan =
    -- This loop holds the server CentralState and reads from the CentralMessage
    -- channel.
    let
        loop :: CentralState -> IO ()
        loop centralState =

            -- first, we wait for the lock to be available (analagous to atomically)
            -- then we read or access the message in the Tchan and then we lock
            -- readTChan has to be completed before any other operation == atomically
            -- binding: process flow of input (fundemental notation == to do)

            atomically (readTChan chan) >>= processCentralMessage centralState >>= loop

        -- for testing, we can later let teachers create the list
        userList = [ (0,("violin","squeak",Supervisor))
                   , (1,("elephant","trunk",User))
                   , (2,("dog","bark",User))
                   ]

        initial :: CentralState
        initial = CentralState
            { clients          = IM.fromList userList
            , name2PassId      = M'.fromList $ map ( \ (id,(name,password,uType)) -> (name,(password,id)) ) userList
            , connected        = IM.empty
            , activities       = IM.empty
            , nextUserId       = 1000
            }
    in
        loop initial

-- App in the occasion that someone fails to connect
fallbackApp :: Application
fallbackApp _ respond = respond $ responseLBS status400 [] "Not a WebSocket request."

-- The main app run by the server
app :: TChan CentralMessage -> Application
app chan = websocketsOr WS.defaultConnectionOptions (wsApp chan) fallbackApp

-- Active component of the app
main :: IO ()
main = do
    Tio.putStrLn "Server starting"
    centralMessageChan <- atomically (newTChan :: STM (TChan CentralMessage))
    forkIO $ processCentralChan centralMessageChan
    Tio.putStrLn "Server listening on 8081"
    run 8081 (app centralMessageChan)

tShow x = T.pack $ show x
