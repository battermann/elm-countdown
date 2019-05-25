module Main exposing (Model, Msg(..), init, main, update, view)

import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Select as Select
import Bootstrap.Grid as Grid
import Bootstrap.Utilities.Flex as Flex
import Bootstrap.Utilities.Spacing as Spacing
import Browser exposing (Document, UrlRequest)
import Browser.Navigation exposing (Key)
import Date exposing (Date)
import Dict
import Html exposing (Html)
import Html.Attributes
import Html.Events
import List.Extra
import Task
import Time exposing (Posix, Zone)
import Time.Extra
import TimeZone
import Url exposing (Url)
import Url.Builder



---- FORM ----


type alias Error =
    String


type alias Validated a =
    Result (List Error) a


type alias Field a =
    { name : String
    , value : String
    , validated : Validated a
    }


type alias EventForm =
    { name : Field String
    , localDate : Field Date
    , timeZone : Field ( String, Zone )
    , hour : Field Int
    , minute : Field Int
    }


setValue : (String -> Validated a) -> String -> Field a -> Field a
setValue f value field =
    { field | value = value, validated = f value }


isValid : EventForm -> Bool
isValid =
    fromForm >> Result.map (always True) >> Result.withDefault False


apply : Validated a -> Validated (a -> b) -> Validated b
apply result fResult =
    case ( fResult, result ) of
        ( Ok f, Ok a ) ->
            Ok <| f a

        ( Err errs, Ok _ ) ->
            Err errs

        ( Ok _, Err errs ) ->
            Err errs

        ( Err errs1, Err errs2 ) ->
            Err (errs1 ++ errs2)


initialForm : EventForm
initialForm =
    { name = { name = "Event Name", value = "", validated = Err [ "Name must not be empty." ] }
    , localDate = { name = "Date", value = "", validated = Err [ "Invalid date." ] }
    , hour = { name = "Hour", value = "00", validated = Ok 0 }
    , minute = { name = "Minute", value = "00", validated = Ok 0 }
    , timeZone = { name = "Time Zone", value = "", validated = Err [ "Time zone is not valid." ] }
    }


setName : String -> EventForm -> EventForm
setName name form =
    let
        f value =
            if value |> String.isEmpty |> not then
                Ok value

            else
                Err [ "Name must not be empty." ]
    in
    { form | name = form.name |> setValue f name }


filterMaybe : (a -> Bool) -> Maybe a -> Maybe a
filterMaybe f =
    Maybe.andThen
        (\a ->
            if f a then
                Just a

            else
                Nothing
        )


withinInclusive : Int -> Int -> Int -> Bool
withinInclusive lower upper value =
    value >= lower && value <= upper


setHour : String -> EventForm -> EventForm
setHour hour form =
    let
        f value =
            String.toInt value
                |> filterMaybe (withinInclusive 0 23)
                |> Result.fromMaybe [ "Hour must be a value between 0 and 23" ]
    in
    { form | hour = form.hour |> setValue f hour }


setMinute : String -> EventForm -> EventForm
setMinute minute form =
    let
        f value =
            String.toInt value
                |> filterMaybe (withinInclusive 0 59)
                |> Result.fromMaybe [ "Minute must be a value between 0 and 59" ]
    in
    { form | minute = form.minute |> setValue f minute }


setDate : String -> EventForm -> EventForm
setDate date form =
    let
        f value =
            Date.fromIsoString value
                |> Result.mapError List.singleton
    in
    { form | localDate = form.localDate |> setValue f date }


setZone : String -> EventForm -> EventForm
setZone zone form =
    let
        f value =
            TimeZone.zones
                |> Dict.get value
                |> Maybe.map (\z -> ( zone, z () ))
                |> Result.fromMaybe [ "Time zone is not valid." ]
    in
    { form | timeZone = form.timeZone |> setValue f zone }



---- MODEL ----


type alias LocalDateTime =
    Time.Extra.Parts


type alias Event =
    { name : String
    , timeZone : ( String, Zone )
    , localDateTime : LocalDateTime
    }


type alias Model =
    { currentTime : Maybe Posix
    , key : Key
    , events : List Event
    , zone : Maybe ( String, Zone )
    , form : EventForm
    }


init : Url -> Key -> ( Model, Cmd Msg )
init url key =
    ( { currentTime = Nothing
      , key = key
      , events = fromUrl url
      , form = initialForm
      , zone = Nothing
      }
    , Cmd.batch [ Task.perform Now Time.now, Task.attempt Here TimeZone.getZone ]
    )


createEvent : String -> Date -> Int -> Int -> ( String, Zone ) -> Event
createEvent name localDate hours minutes zone =
    let
        localDateTime =
            { year = Date.year localDate
            , month = Date.month localDate
            , day = Date.day localDate
            , hour = hours
            , minute = minutes
            , second = 0
            , millisecond = 0
            }
    in
    Event name zone localDateTime


fromForm : EventForm -> Validated Event
fromForm form =
    Ok createEvent
        |> apply form.name.validated
        |> apply form.localDate.validated
        |> apply form.hour.validated
        |> apply form.minute.validated
        |> apply form.timeZone.validated



---- ROUTING ----


fromQueryParameter : String -> Maybe Event
fromQueryParameter query =
    case query |> String.split "=" |> List.filterMap Url.percentDecode of
        name :: event :: [] ->
            case String.split "@" event of
                posix :: zone :: [] ->
                    TimeZone.zones
                        |> Dict.get zone
                        |> Maybe.map (\z -> z ())
                        |> Maybe.map (Tuple.pair zone)
                        |> Maybe.andThen
                            (\z ->
                                posix
                                    |> String.toInt
                                    |> Maybe.map Time.millisToPosix
                                    |> Maybe.map (Time.Extra.posixToParts (Tuple.second z))
                                    |> Maybe.map (Event name z)
                            )

                _ ->
                    Nothing

        _ ->
            Nothing


fromUrl : Url -> List Event
fromUrl { query } =
    query
        |> Maybe.map (String.split "&" >> List.filterMap fromQueryParameter)
        |> Maybe.withDefault []


toQueryParam : Event -> Url.Builder.QueryParameter
toQueryParam event =
    [ Time.Extra.partsToPosix (Tuple.second event.timeZone) event.localDateTime
        |> Time.posixToMillis
        |> String.fromInt
    , Tuple.first event.timeZone
    ]
        |> String.join "@"
        |> Url.Builder.string event.name


toUrl : List Event -> String
toUrl events =
    Url.Builder.absolute [] (events |> List.map toQueryParam)


deleteUrl : Int -> List Event -> String
deleteUrl index =
    List.Extra.removeAt index >> toUrl



---- UPDATE ----


type Msg
    = Tick Posix
    | Now Posix
    | Here (Result TimeZone.Error ( String, Zone ))
    | ChangedUrl Url
    | ClickedLink UrlRequest
    | NameInput String
    | HourInput String
    | MinuteInput String
    | DateInput String
    | ZoneInput String
    | Submit


type alias Diff =
    { days : Int
    , hours : Int
    , minutes : Int
    , seconds : Int
    }


divWithRemainder : Int -> Int -> ( Int, Int )
divWithRemainder a b =
    ( a // b, a |> remainderBy b )


diff : Posix -> Event -> Diff
diff now event =
    let
        sec =
            1000

        min =
            60 * sec

        hour =
            60 * min

        day =
            24 * hour

        diffMillis =
            Time.posixToMillis (Time.Extra.partsToPosix (Tuple.second event.timeZone) event.localDateTime) - Time.posixToMillis now

        ( days, hoursLeft ) =
            divWithRemainder diffMillis day

        ( hours, minsLeft ) =
            divWithRemainder hoursLeft hour

        ( minutes, secsLeft ) =
            divWithRemainder minsLeft min

        ( seconds, _ ) =
            divWithRemainder secsLeft sec
    in
    { days = days, hours = hours, minutes = minutes, seconds = seconds }


trySetDate : Maybe Posix -> Maybe Zone -> EventForm -> EventForm
trySetDate maybePosix maybeZone form =
    Maybe.map2 Time.Extra.posixToParts maybeZone maybePosix
        |> Maybe.map
            (\d ->
                form
                    |> setDate
                        ([ String.fromInt d.year
                         , String.padLeft 2 '0' (String.fromInt (Date.monthToNumber d.month))
                         , String.padLeft 2 '0' (String.fromInt (d.day + 1))
                         ]
                            |> String.join "-"
                        )
            )
        |> Maybe.withDefault form


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Now time ->
            ( { model
                | currentTime = Just time
                , form = model.form |> trySetDate (Just time) (model.zone |> Maybe.map Tuple.second)
              }
            , Cmd.none
            )

        Tick time ->
            ( { model | currentTime = Just time }, Cmd.none )

        Here (Ok zone) ->
            ( { model
                | zone = Just zone
                , form =
                    model.form
                        |> setZone (Tuple.first zone)
                        |> trySetDate model.currentTime (Just (Tuple.second zone))
              }
            , Cmd.none
            )

        Here (Err _) ->
            ( { model | zone = Nothing }, Cmd.none )

        NameInput name ->
            ( { model | form = model.form |> setName name }, Cmd.none )

        DateInput date ->
            ( { model | form = model.form |> setDate date }, Cmd.none )

        HourInput hour ->
            ( { model | form = model.form |> setHour hour }, Cmd.none )

        MinuteInput minute ->
            ( { model | form = model.form |> setMinute minute }, Cmd.none )

        ZoneInput zone ->
            ( { model | form = model.form |> setZone zone }, Cmd.none )

        ClickedLink request ->
            case request of
                Browser.Internal url ->
                    ( model, Browser.Navigation.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Browser.Navigation.load href )

        ChangedUrl url ->
            ( { model | events = fromUrl url, form = initialForm }, Task.attempt Here TimeZone.getZone )

        Submit ->
            model.form
                |> fromForm
                |> Result.map (\event -> Browser.Navigation.pushUrl model.key (toUrl (event :: model.events)))
                |> Result.withDefault Cmd.none
                |> Tuple.pair model



---- VIEW ----


viewInput : Model -> Html Msg
viewInput model =
    Form.form [ Html.Events.onSubmit Submit ]
        [ Form.group []
            [ Form.label [ Html.Attributes.for "eventname" ] [ Html.text model.form.name.name ]
            , Input.text
                [ Input.id "eventname"
                , Input.onInput NameInput
                , Input.value model.form.name.value
                , Input.placeholder "Event Name"
                ]
            ]
        , Form.row []
            [ Form.col []
                [ Form.group []
                    [ Form.label [ Html.Attributes.for "eventdate" ] [ Html.text model.form.localDate.name ]
                    , Input.date
                        [ Input.id "eventdate"
                        , Input.value model.form.localDate.value
                        , Input.onInput DateInput
                        ]
                    ]
                ]
            , Form.col []
                [ Form.group []
                    [ Form.label [ Html.Attributes.for "eventhour" ] [ Html.text model.form.hour.name ]
                    , Select.custom
                        [ Select.id "eventhour"
                        , Select.onChange HourInput
                        , Select.attrs [ Html.Attributes.style "min-width" "75px" ]
                        ]
                        (List.range
                            0
                            23
                            |> List.map (String.fromInt >> String.padLeft 2 '0')
                            |> List.map (\hour -> Select.item [ Html.Attributes.selected (hour == model.form.hour.value) ] [ Html.text hour ])
                        )
                    ]
                ]
            , Form.col []
                [ Form.group []
                    [ Form.label [ Html.Attributes.for "eventminute" ] [ Html.text model.form.minute.name ]
                    , Select.custom
                        [ Select.id "eventminute"
                        , Select.onChange MinuteInput
                        , Select.attrs [ Html.Attributes.style "min-width" "75px" ]
                        ]
                        (List.range
                            0
                            59
                            |> List.map (String.fromInt >> String.padLeft 2 '0')
                            |> List.map (\minute -> Select.item [ Html.Attributes.selected (minute == model.form.minute.value) ] [ Html.text minute ])
                        )
                    ]
                ]
            , Form.col []
                [ Form.group []
                    [ Form.label [ Html.Attributes.for "eventtimezone" ] [ Html.text model.form.timeZone.name ]
                    , Select.custom
                        [ Select.id "eventtimezone"
                        , Select.onChange ZoneInput
                        , Select.attrs [ Html.Attributes.style "min-width" "80px" ]
                        ]
                        (TimeZone.zones
                            |> Dict.keys
                            |> List.map
                                (\zone ->
                                    Select.item
                                        [ Html.Attributes.selected (zone == model.form.timeZone.value)
                                        , Html.Attributes.value zone
                                        ]
                                        [ Html.text (zone |> String.replace "_" " ") ]
                                )
                        )
                    ]
                ]
            ]
        , Button.button [ Button.primary, Button.disabled (not (model.form |> isValid)) ] [ Html.text "Submit" ]
        ]


viewCountdown : String -> Int -> Html Msg
viewCountdown name value =
    Html.div []
        [ Html.div
            [ Html.Attributes.style "font-size" "50px"
            , Html.Attributes.style "text-align" "center"
            ]
            [ Html.text (String.padLeft 2 '0' (String.fromInt value)) ]
        , Html.div [ Html.Attributes.style "text-align" "center", Html.Attributes.style "font-size" "12px" ] [ Html.text name ]
        ]


viewEvent : String -> Event -> Posix -> Card.Config Msg
viewEvent deleteLink event currentTime =
    let
        d =
            diff currentTime event

        colon =
            Html.div
                [ Html.Attributes.style "font-size" "50px"
                , Html.Attributes.style "text-align" "center"
                ]
                [ Html.text ":" ]
    in
    Card.config [ Card.attrs [ Spacing.mb3 ] ]
        |> Card.headerH3 [] [ Html.text event.name ]
        |> Card.block []
            [ Block.text []
                [ Html.div [ Flex.block, Flex.row, Flex.justifyCenter ]
                    [ viewCountdown "DAYS" d.days
                    , colon
                    , viewCountdown "HOURS" d.hours
                    , colon
                    , viewCountdown "MINUTES" d.minutes
                    , colon
                    , viewCountdown "SECONDS" d.seconds
                    ]
                ]
            ]
        |> Card.footer [] [ Button.linkButton [ Button.danger, Button.attrs [ Html.Attributes.href deleteLink ] ] [ Html.text "Delete" ] ]


viewCountdowns : Model -> Html Msg
viewCountdowns model =
    let
        cardList =
            model.events
                |> List.sortBy (\event -> Time.Extra.partsToPosix (Tuple.second event.timeZone) event.localDateTime |> Time.posixToMillis)
                |> List.indexedMap Tuple.pair
                |> List.filterMap (\( index, event ) -> model.currentTime |> Maybe.map (viewEvent (deleteUrl index model.events) event))

        input =
            Card.config [ Card.attrs [ Spacing.mb3 ] ]
                |> Card.headerH3 [] [ Html.text "New Event" ]
                |> Card.block [] [ Block.text [] [ viewInput model ] ]
    in
    Html.div [] (cardList ++ [ input ] |> List.map Card.view)


view : Model -> Document Msg
view model =
    { title = "Elm Countdown"
    , body =
        [ Grid.container []
            [ CDN.stylesheet -- creates an inline style node with the Bootstrap CSS
            , Grid.row []
                [ Grid.col []
                    [ Html.h1 [ Spacing.mb5 ] [ Html.text "Elm Countdown" ]
                    , viewCountdowns model
                    ]
                ]
            ]
        ]
    }



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.application
        { view = view
        , init = always init
        , update = update
        , subscriptions = always (Time.every 100 Tick)
        , onUrlChange = ChangedUrl
        , onUrlRequest = ClickedLink
        }
