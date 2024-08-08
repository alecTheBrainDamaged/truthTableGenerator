module Main exposing (..)

import Element as E exposing (..)
import Element.Border as B exposing (..)
import Element.Input as EI exposing (..)
import Element.Events exposing (onClick)
import Element.Background as Background exposing (..)
import Html exposing (..)
import Http 
import Element.Font as Font exposing (..)
import Json.Decode as D exposing (..)
import Json.Encode as E exposing (..)
import Browser

main = Browser.element
     { init = init
     , view = \model -> E.layout [] (E.el [E.width fill, E.height fill, Background.color millennialPink] (view model))
     , update = update
     , subscriptions = subscriptions
     }
millennialPink : Color
millennialPink = rgb255 243 207 198
type alias Model =
   { status : Status
   , argument : String
   }

type Status
   = Failure (Maybe String, Maybe Int)
   | Loading 
   | Success HaskellServerResponse
   | Default

init : () -> (Model , Cmd Msg)
init _ = 
   ( 
     {status = Default
     , argument = ""
     }
   , Cmd.none
   )
---- OUR MSGS
type Msg = SendArgHttpPost
         | GotJson (Result Http.Error HaskellServerResponse)
         | UpdateArgument String
------- THIS IS part of our VIEW MODULE, What in the FUCK we are RENDERING ON SCREEN. 
title : String
title = "Truth Table Generator"


inputArgumentBox : Element Msg
inputArgumentBox = 
      EI.multiline [] { onChange = UpdateArgument
                      , text = ""
                      , placeholder = Nothing
                      , label = EI.labelAbove [] (E.text "Prop box")
                      , spellcheck = False
                      }

submitArg : Element Msg
submitArg = EI.button [pointer] { onPress = Just SendArgHttpPost, label = E.text "Submit"}

defaultMsg : Element Msg
defaultMsg = column [spacing 10]
             [ inputArgumentBox
             , submitArg
             ]

colorRed : Color
colorRed = rgb255 255 0 0 

colorGreen : Color
colorGreen = rgb255 0 255 0


view : Model -> Element Msg
view model =
  column [spacing 20, E.centerX, E.centerY]
  [
   case model.status of
    Default -> column [spacing 10] 
               [E.text title
               ,E.text "Your natural language argument: "
               , E.text model.argument
               , defaultMsg
               ]
    Loading -> column [spacing 10] [E.text "loading argument", defaultMsg]
    Failure tuple  ->  column [spacing 10] 
                     [
                     case (Tuple.second tuple) of
                        Nothing -> column [spacing 10] 
                                   [ E.text title
                                   , E.text "Your natural language argument: "
                                   , E.text model.argument
                                   , defaultMsg
                                   , E.text "Error found"
                                   , E.el [] E.none
                                   , E.text (Maybe.withDefault "" (Tuple.first tuple))
                                   ]
                        (Just i) -> column [spacing 10]
                                     [ E.text title
                                     , E.text "Your natural language argument: "
                                     , E.text model.argument                                     
                                     , defaultMsg
                                     , E.text "Error found"
                                     , E.el [] E.none
                                     , E.text (Maybe.withDefault "" (Tuple.first tuple) ++ " Integer: " ++ (String.fromInt i)) 
                                     ]
                     ]
    Success h -> case String.isEmpty h.err of
                  True -> column [spacing 15]
                          [ E.text title
                          , E.text "Your natural language argument: "
                          , E.text model.argument
                          , defaultMsg
                          , createTruthTable h
                          , E.text "Argument's validity"
                          , showValidity h.validityy
                          ]

                  _    -> column [spacing 15]
                          [ E.text title
                          , E.text "Your natural language argument: "
                          , E.text model.argument
                          , defaultMsg
                          , E.text "There was a parsing error"
                          , E.text "Sorry :("
                          , E.text h.err
                          ]
   ]
showValidity : String -> Element Msg
showValidity s =
  case s of
   "Invalid" -> E.el
                  [Font.color colorRed
                  ,Font.size 18
                  , Font.center
                  , Font.family
                    [Font.typeface "Open Sans"
                    ,Font.sansSerif
                    ]
                  ]
                   (E.text s)
   _         -> E.el
                  [Font.color colorGreen
                  ,Font.size 18
                  , Font.center
                  , Font.family
                    [Font.typeface "Open Sans"
                    ,Font.sansSerif
                    ]
                  ]
                   (E.text s)


---------------------


{-
                 column [spacing 15]
               [ title
               , defaultMsg
               , E.text <| "headers: " ++ h.headers
               , E.text <| "assignments: " ++ h.assignments
               , E.text <| "premiseEval: " ++ h.premiseEval
               , E.text <| "conclusionEval: " ++ h.conclusionEval
               , E.text <| "validityy: " ++ h.validityy
               ]
-}


{-
type alias HaskellServerResponse =
      { err : String
      , headers : String
      , assignments : String
      , premiseEval : String
      , conclusionEval : String
      , validityy : String
      }
-}

-- THis is our response that we make from the Json
type alias HaskellServerResponse =
      { err : String
      , headers : List String
      , assignments : List (String, List Bool)
      , premiseEval : List (List (String,Bool))
      , conclusionEval : List (String, Bool)
      , validityy : String
      }

--- OUR UPDATE FUNCTION 
{-
our update function pattern matches on the msg

pattern matching on MSG


STATUS UPDATES to the MODEL 
HERE WE HAVE OUR MODEL
type alias Model =
   { status : Status
   , argument : String
   }
AND ALL THE STATUSES
type Status
   = Failure (Maybe String, Maybe Int)
   | Loading 
   | Success HaskellServerResponse
   | Default
AND here are all the messages. 
type Msg = SendArgHttpPost
         | GotJson (Result Http.Error HaskellServerResponse)
         | UpdateArgument String
We pattern match on Msg
[
if we are given a SendArgHttpPost Msg, we update our model Status to Loading, and send out a Http.post request. 
the post request will send our ARGUMENT as a Json Body and return a GotJson Msg
If we are given a GotJson as our message we pattern match on the result, if it is Ok, we update the model Status as 
Success response, 
if there is an error, we pattern match on the error and update our model with Failure Status. 
]
ARGUMENT UPDATES to the MODEL

[
after pattern matching on msg, if we get an update argument, then we see what the new argument is, and we update our model's Argument. 
]
-}
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
   SendArgHttpPost -> ( {model | status = Loading}
                 , Http.post
                 { url = "/api/submit"
                 , body = Http.jsonBody (E.object [("argument",E.string model.argument)])
                 , expect = Http.expectJson GotJson haskellResponseDecoder
                 } 
                 )
   GotJson result ->
    case result of
     Ok response ->
      ({model | status = Success response}, Cmd.none)
     Err error   ->
         case error of
          (Http.BadUrl s) ->  
                ({model | status = Failure (Just s, Nothing)} , Cmd.none)
          Http.Timeout -> 
               ({model | status = Failure (Just "Timeout",Nothing)}, Cmd.none)
          Http.NetworkError -> 
               ({model | status = Failure (Just "NetworkError",Nothing)} ,Cmd.none)
          (Http.BadStatus i) -> 
               ({model | status = Failure (Just "BadStatus", Just i)} , Cmd.none)
          (Http.BadBody s) -> 
               ({model | status = Failure (Just ("Badbody " ++ s), Nothing) }, Cmd.none) 

   UpdateArgument newArgument ->
    ({model | argument = newArgument}, Cmd.none)
--------------------------------------------------
-- THIS IS OUR DATA TYPE FOR CREATING A TABLE
type alias Argument =
      { assignment : (String, List Bool)
      , propositions : List (String, Bool)
      , conclusion  : Maybe (String, Bool)
      }

------
----- CREATING TRUTH TABLE FROM RESPONSE FROM BACK END -------------


black : Color
black = rgb255 0 0 0 

createTruthTable : HaskellServerResponse -> Element Msg
createTruthTable h =
    let vars = Tuple.first (Maybe.withDefault ("",[]) (List.head (h.assignments)))
        isArg : Bool
        isArg = case h.validityy of 
                 "Nothing" -> False
                 _         -> True 
        args = createArguments h isArg                 
  
    in E.table [B.width 1, B.solid, (B.color black)]
       { data = args
       , columns = createColumns vars h.headers 
       }

-- foldr : (a -> b -> b) -> b -> List a -> b

createArguments : HaskellServerResponse -> Bool -> List Argument
createArguments h isArgument = 
      case h.assignments of
       [] -> []
       _ -> let firstAssignment = Maybe.withDefault ("",[] ) (List.head h.assignments)
                headProp        = Maybe.withDefault [] (List.head h.premiseEval)
                conclusioN      =   case isArgument of
                                     True -> Just (Maybe.withDefault ("",False) (List.head h.conclusionEval))
                                     False -> Nothing
                arg = { assignment   = firstAssignment
                      , propositions = headProp
                      , conclusion   = conclusioN
                      }
                newResponse = 
                   {h | assignments = Maybe.withDefault [] (List.tail h.assignments) , premiseEval = Maybe.withDefault [] (List.tail h.premiseEval), conclusionEval = Maybe.withDefault [] (List.tail h.conclusionEval)}
            in arg :: (createArguments newResponse isArgument)

stylePropositions : String -> Element Msg
stylePropositions s = E.el
                  [Font.color black
                  , Font.italic
                  , Font.family
                    [Font.typeface "Open Sans"
                    ,Font.sansSerif
                    ]
                  ]
                   (E.text s)
createColumns : String -> List String -> List (Column Argument Msg)
createColumns vars headerS =   
  let tailify : String -> String
      tailify s =  String.fromList (Maybe.withDefault [] (List.tail (String.toList s)))
      tailifyList : List a -> List a
      tailifyList l = Maybe.withDefault [] (List.tail l) 
  in case headerS of
      [] -> []
      [h] -> List.singleton { header = column [B.width 1, B.solid, B.color black] [E.el [E.centerX, E.centerY] (stylePropositions h)]
                            , width = fill
                            , view = \arg -> column [B.width 1, B.solid, B.color black] [viewMaybeConc h arg]
                            }
      (headerr :: headerss) -> case List.isEmpty (String.toList vars) of
                                True -> {header = column [B.width 1, B.solid, B.color black] [E.el [E.centerX, E.centerY] (stylePropositions headerr)]
                                        , width = fill
                                        , view  = \arg -> column [B.width 1, B.solid, B.color black] [viewProp headerr arg]
                                        } :: (createColumns (tailify vars) (tailifyList headerS) )
                                False -> {header = column [B.width 1, B.solid, B.color black] [E.el [E.centerX, E.centerY] (E.text headerr)]
                                         , width = fill
                                         , view  = \arg -> column [B.width 1, B.solid, B.color black] [viewVar headerr arg]
                                         } :: (createColumns (tailify vars) (tailifyList headerS))

toChar : String -> Char
toChar s =
     case (String.toList s) of
      [x] -> x
      _   -> '?'

getIndex : Char -> String -> Maybe Int
getIndex x l = g x (String.toList l) (List.length (String.toList l))
g : a -> List a -> Int -> Maybe Int 
g x l i = case l of
           [] -> Nothing
           (y :: ys) -> case x == y of
                         True  -> Just (i - (List.length ys) - 1)
                         False -> g x ys i                  

index : Int -> List a -> Maybe a
index i l =
   case (i < 0) || (i > List.length l) of
    True -> Nothing
    False -> case i == 0 of
              True -> case l of
                       [] -> Nothing
                       (x :: xs) -> Just x
              False -> case l of
                        [] -> Nothing
                        (x :: xs) -> index (i - 1) xs

fromBool : Bool -> String
fromBool b = 
   case b of
    True -> "T"
    False -> "F"

viewVar : String -> Argument -> Element Msg
viewVar var arg = 
   let indexOfVar  = Maybe.withDefault (-1) (getIndex (toChar var) (Tuple.first arg.assignment))
       boolean     = Maybe.withDefault True (index indexOfVar (Tuple.second arg.assignment))
   in  E.el [E.centerX, E.centerY] (E.text (fromBool boolean))

viewMaybeConc : String -> Argument -> Element Msg
viewMaybeConc maybeConc arg = 
     case arg.conclusion of
      Nothing -> viewProp maybeConc arg
      (Just t) -> E.el [E.centerX, E.centerY] (E.text <| fromBool <| Tuple.second t)

viewProp : String -> Argument -> Element Msg
viewProp prop arg = 
     let props : List (String,Bool)
         props = arg.propositions
         findMyProp : List (String, Bool) -> String -> List (String, Bool)
         findMyProp l s = List.filter (\(x,y) -> s == x) l
         myPropDouble : (String,Bool)
         myPropDouble = case (findMyProp props prop) of
                         []    -> ("couldNotFindProp",False)
                         [d]   -> d
                         (d :: ds) -> ("Dumbass you wrote the prop more than once" ++ (Tuple.first d), Tuple.second d)
         myPropBoolean : Bool
         myPropBoolean = Tuple.second myPropDouble
     in 
      E.el [E.centerX, E.centerY](E.text (fromBool myPropBoolean))
--- ENDING OF CREATING TRUTH TABLE
-----------------------------------------                            
--- JSON DECODER FOR HASKELL BACKEND
haskellResponseDecoder : Decoder HaskellServerResponse
haskellResponseDecoder =
    D.map6 HaskellServerResponse 
        (D.field "err" D.string)
        (D.field "headers" (D.list D.string))
        (D.field "assignments" (D.list (D.map2 Tuple.pair (D.index 0 D.string) (D.index 1 (D.list D.bool)))))
        (D.field "premiseEval" (D.list (D.list (D.map2 Tuple.pair (D.index 0 D.string) (D.index 1 D.bool)))))
        (D.field "conclusionEval" (D.list (D.map2 Tuple.pair (D.index 0 D.string) (D.index 1 D.bool))))
        (D.field "validityy" D.string)
---
{-
haskellResponseDecoder : Decoder HaskellServerResponse
haskellResponseDecoder =
      D.map6 HaskellServerResponse
        (D.field "err" D.string)
        (D.field "headers"  D.string)
        (D.field "assignments" D.string)
        (D.field "premiseEval"  D.string)
        (D.field "conclusionEval" D.string)
        (D.field "validityy" D.string)

 -}       
-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model = Sub.none
--
