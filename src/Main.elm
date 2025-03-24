module Main exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Http
import List.Extra
import Port
import Random
import Random.Extra
import Random.List
import Svg
import Svg.Attributes as SA


type alias Model =
    { words : Result Http.Error (List String)
    , currentWord : String
    , currentName : String
    , previousWord : Maybe String
    , favorites : List String
    }


init : () -> ( Model, Cmd Msg )
init flags =
    ( { words = Err (Http.BadUrl "")
      , currentWord = ""
      , currentName = ""
      , previousWord = Nothing
      , favorites = []
      }
    , Cmd.batch
        [ fetchWords
        , Port.getFavorites ()
        ]
    )


type Msg
    = GotWords (Result Http.Error String)
    | GotRandomWord String
    | GotRandomName String
    | GotNewSeed Random.Seed
    | ClickedGenerateWord
    | ClickedAddToFavorite
    | ClickedRemoveFromFavorites String
    | ClickedTextToSpeech
    | ClickedToggleFavorites
    | Port_GotFavorites (List String)


fetchWords : Cmd Msg
fetchWords =
    Http.get
        { url = "https://gist.githubusercontent.com/alexanderwassberg/4614feaf1258c11bec7170ece37db610/raw/b6d68a8f26563cd6b237e67fdc4774887b147ed2/words.txt"
        , expect = Http.expectString GotWords
        }


names : List String
names =
    [ "Bosse"
    , "Conny"
    , "Urban"
    , "Nisse"
    , "Albert"
    , "Tommy"
    , "Lukas"
    , "Philip"
    , "Alexander"
    , "Hans"
    , "Olof"
    , "Erik"
    , "Johan"
    , "Björn"
    , "Sven"
    , "Magnus"
    , "Gunnar"
    , "Kristoffer"
    , "Fredrik"
    , "Mats"
    , "Anders"
    , "Jan"
    , "Henrik"
    , "Stefan"
    , "Carl"
    , "Emil"
    , "Oskar"
    , "Vincent"
    , "Göran"
    ]


randomNameGenerator : Random.Generator String
randomNameGenerator =
    let
        randomIndexGenerator =
            Random.int 0 (List.length names - 1)
    in
    Random.map
        (\idx ->
            case List.Extra.getAt idx names of
                Just name ->
                    name

                Nothing ->
                    ""
        )
        randomIndexGenerator


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotWords result ->
            case result of
                Ok data ->
                    ( { model
                        | words = Ok (String.split "\n" data)
                      }
                    , generateSeed
                    )

                Err err ->
                    ( { model | words = Err err }, Cmd.none )

        GotRandomWord word ->
            ( { model
                | currentWord = word
              }
            , Random.generate GotRandomName randomNameGenerator
            )

        GotRandomName name ->
            let
                updatedName : String -> String
                updatedName str =
                    if String.isEmpty str then
                        str

                    else
                        case String.toList (String.reverse str) of
                            firstChar :: _ ->
                                if firstChar == 's' then
                                    str

                                else
                                    str ++ "s"

                            _ ->
                                str
            in
            ( { model
                | currentName = updatedName name
              }
            , Cmd.none
            )

        ClickedGenerateWord ->
            case model.words of
                Ok _ ->
                    ( { model
                        | previousWord = Just model.currentWord
                      }
                    , generateSeed
                    )

                Err _ ->
                    ( model, Cmd.none )

        GotNewSeed seed ->
            case model.words of
                Ok wordList ->
                    let
                        ( lefts, rights ) =
                            splitWord wordList
                    in
                    ( model, shuffleAndCombineWithSeed lefts rights seed )

                Err _ ->
                    ( model, Cmd.none )

        ClickedAddToFavorite ->
            ( model
            , if List.member model.currentWord model.favorites then
                Port.removeFavorite model.currentWord
              else
                Port.setFavorite model.currentWord
            )

        ClickedRemoveFromFavorites word ->
            ( model
            , Port.removeFavorite word
            )

        Port_GotFavorites favorites ->
            ( { model | favorites = favorites }
            , Cmd.none
            )

        ClickedTextToSpeech ->
            ( model
            , Port.sendTTS model.currentWord
            )

        ClickedToggleFavorites ->
            ( model
            , Port.toggleDialog ()
            )


generateSeed : Cmd Msg
generateSeed =
    Random.generate GotNewSeed Random.independentSeed


shuffleAndCombineWithSeed : List String -> List String -> Random.Seed -> Cmd Msg
shuffleAndCombineWithSeed lefts rights seed =
    let
        ( shuffledLefts, leftSeed ) =
            Random.step (Random.List.shuffle lefts) seed

        ( shuffledRights, _ ) =
            Random.step (Random.List.shuffle rights) leftSeed
    in
    Random.generate GotRandomWord <|
        Random.constant <|
            case ( shuffledLefts, shuffledRights ) of
                ( l :: _, r :: _ ) ->
                    l ++ "-" ++ r

                _ ->
                    ""


splitWord : List String -> ( List String, List String )
splitWord list =
    List.foldr
        (\word ( lefts, rights ) ->
            case String.split "-" word of
                [ left, right ] ->
                    ( capitalize left :: lefts, right :: rights )

                _ ->
                    ( lefts, rights )
        )
        ( [], [] )
        list


view : Model -> Html Msg
view model =
    case model.words of
        Ok _ ->
            Html.node "ordflaket"
                []
                [ Html.header
                    []
                    [ logo
                    , Html.button
                         [ HE.onClick ClickedToggleFavorites
                         ]
                         [ heartFill
                         , Html.text "Favorites"
                         ]
                    ]
                , view_Word model
                , Html.footer
                    []
                    [ Html.button
                        [ HE.onClick ClickedGenerateWord
                        , HA.class "btn-generate"
                        ]
                        [ Html.text "Generera nytt ord"
                        ]
                    , Html.button
                        [ HA.class "btn-favorite"
                        , HA.classList [ ( "active", List.member model.currentWord model.favorites ) ]
                        , HE.onClick ClickedAddToFavorite
                        ]
                        [ if List.member model.currentWord model.favorites then
                            heartFill

                          else
                            heart
                        ]
                    , Html.button
                        [ HA.class "btn-tts"
                        , HE.onClick ClickedTextToSpeech
                        ]
                        [ speaker
                        ]
                    ]
                -- , Html.node "custom-dialog"
                --     []
                --     [ Html.ul
                --         [ HA.class "favorites"
                --         ]
                --         (List.map
                --             (\favorite ->
                --                 Html.li
                --                     []
                --                     [ Html.text favorite
                --                     , Html.button
                --                         [ HE.onClick (ClickedRemoveFromFavorites favorite)
                --                         ]
                --                         [ close
                --                         ]
                --                     ]
                --             )
                --             model.favorites
                --         )
                --     ]
                ]

        Err err ->
            Html.text (errorToString err)


view_Word : Model -> Html Msg
view_Word model =
    Html.h2
        []
        [ Html.text model.currentWord
        ]


view_Name : Model -> Html Msg
view_Name model =
    Html.div
        []
        [ Html.strong
            []
            [ Html.text model.currentName
            ]
        , Html.h3
            []
            [ model.currentWord
                |> String.split "-"
                |> List.intersperse " & "
                |> String.concat
                |> Html.text
            ]
        ]


view_FavoriteButton : Model -> Html Msg
view_FavoriteButton model =
    Html.button
        [ HA.class "btn-favorite"
        , HA.classList [ ( "active", List.member model.currentWord model.favorites ) ]
        , HE.onClick ClickedAddToFavorite
        ]
        [ if List.member model.currentWord model.favorites then
            heartFill

          else
            heart
        ]


errorToString : Http.Error -> String
errorToString error =
    case error of
        Http.BadUrl message ->
            "Loading..."

        Http.Timeout ->
            "Timeout"

        Http.NetworkError ->
            "Network error"

        Http.BadStatus code ->
            "Error: " ++ String.fromInt code

        Http.BadBody message ->
            message


capitalize : String -> String
capitalize str =
    case String.uncons str of
        Just ( first, rest ) ->
            String.fromChar (Char.toUpper first) ++ rest

        Nothing ->
            ""


heart : Html Msg
heart =
    Svg.svg
        [ SA.viewBox "2.03 3.04 19.95 17.99"
        , SA.fill "currentColor"
        ]
        [ Svg.path
            [ SA.d "M12 4.595a5.904 5.904 0 0 0-3.996-1.558 5.942 5.942 0 0 0-4.213 1.758c-2.353 2.363-2.352 6.059.002 8.412l7.332 7.332c.17.299.498.492.875.492a.99.99 0 0 0 .792-.409l7.415-7.415c2.354-2.354 2.354-6.049-.002-8.416a5.938 5.938 0 0 0-4.209-1.754A5.906 5.906 0 0 0 12 4.595zm6.791 1.61c1.563 1.571 1.564 4.025.002 5.588L12 18.586l-6.793-6.793c-1.562-1.563-1.561-4.017-.002-5.584.76-.756 1.754-1.172 2.799-1.172s2.035.416 2.789 1.17l.5.5a.999.999 0 0 0 1.414 0l.5-.5c1.512-1.509 4.074-1.505 5.584-.002z"
            ]
            []
        ]


heartFill : Html Msg
heartFill =
    Svg.svg
        [ SA.viewBox "2.03 3.04 19.95 18.38"
        , SA.fill "currentColor"
        ]
        [ Svg.path
            [ SA.d "M20.205 4.791a5.938 5.938 0 0 0-4.209-1.754A5.906 5.906 0 0 0 12 4.595a5.904 5.904 0 0 0-3.996-1.558 5.942 5.942 0 0 0-4.213 1.758c-2.353 2.363-2.352 6.059.002 8.412L12 21.414l8.207-8.207c2.354-2.353 2.355-6.049-.002-8.416z"
            ]
            []
        ]


speaker : Html Msg
speaker =
    Svg.svg
        [ SA.viewBox "2 3 20 18"
        , SA.fill "currentColor"
        ]
        [ Svg.path
            [ SA.d "M16 21c3.527-1.547 5.999-4.909 5.999-9S19.527 4.547 16 3v2c2.387 1.386 3.999 4.047 3.999 7S18.387 17.614 16 19v2z"
            ]
            []
        , Svg.path
            [ SA.d "M16 7v10c1.225-1.1 2-3.229 2-5s-.775-3.9-2-5zM4 17h2.697l5.748 3.832a1.004 1.004 0 0 0 1.027.05A1 1 0 0 0 14 20V4a1 1 0 0 0-1.554-.832L6.697 7H4c-1.103 0-2 .897-2 2v6c0 1.103.897 2 2 2zm0-8h3c.033 0 .061-.016.093-.019a1.027 1.027 0 0 0 .38-.116c.026-.015.057-.017.082-.033L12 5.868v12.264l-4.445-2.964c-.025-.017-.056-.02-.082-.033a.986.986 0 0 0-.382-.116C7.059 15.016 7.032 15 7 15H4V9z"
            ]
            []
        ]


logo : Html Msg
logo =
    Svg.svg
        [ SA.fill "none"
        , SA.viewBox "0.99 0.35 319.01 98.45"
        ]
        [ Svg.node "script"
            [ HA.attribute "src" "chrome-extension://fjnbnpbmkenffdnngjfgmeleoegfcffe/static/js/contentInt.js"
            ]
            []
        , Svg.path
            [ SA.d "M72.8203 40.2813C80.8412 38.276 89.1094 36.7526 97.625 35.7109L99.2266 45.0078L82.3516 46.8438L82.8203 59.3047L97.1563 58.6797L96.7656 67.9766L82.9766 67.5469L82.3516 87L73.9531 87.7031L72.8203 40.2813Z"
            , SA.fill "#DCDCDC"
            ]
            []
        , Svg.path
            [ SA.d "M107.859 37.7813L117.703 36.2188V57.625L116.492 76.6875C118.497 77.4688 121.206 78.0938 124.617 78.5625C128.029 79.0052 130.086 79.2787 130.789 79.3828L128.563 88.0547C125.776 87.6901 123.523 87.3516 121.805 87.0391L117.234 86.1797C112.521 85.2682 109.396 84.4479 107.859 83.7188V37.7813Z"
            , SA.fill "#DCDCDC"
            ]
            []
        , Svg.path
            [ SA.d "M157 74.0703H154.422C152.964 74.0703 150.659 73.7708 147.508 73.1719L144.539 87.625L135.242 85.5547L139.813 65.2422L145.125 38.6797C151.01 37.638 157.221 36.8438 163.758 36.2969L169.93 63.0938L174.695 86.6094L165.203 88.1328L161.023 65.0859L156.961 45.6328C155.477 45.6328 153.927 45.8802 152.313 46.375L148.953 65.6328L159.031 65.8672L157.82 74.0313C157.534 74.0573 157.26 74.0703 157 74.0703Z"
            , SA.fill "#DCDCDC"
            ]
            []
        , Svg.path
            [ SA.d "M183.484 37.5469L193.211 36.2188L193.484 61.4922L195.32 61.8047L210.008 35.7891L218.523 40.8281C217.039 44.0313 215.177 47.2344 212.938 50.4375L208.016 57.3125L205.047 61.1016C204.109 62.2734 203.237 63.276 202.43 64.1094C204.487 66.2188 206.023 67.8464 207.039 68.9922C212.169 74.9818 215.828 79.7865 218.016 83.4063L210.008 89.2266L196.492 68.4453L193.172 69.9688L192.508 87L183.797 87.7813L183.484 37.5469ZM189.07 71.8047C189.07 71.8307 189.07 71.8438 189.07 71.8438C189.096 71.8438 189.109 71.8307 189.109 71.8047H189.07Z"
            , SA.fill "#DCDCDC"
            ]
            []
        , Svg.path
            [ SA.d "M235.242 78.2891L254.188 81.1406L252.117 89.6563L246.219 88.875C237.547 87.6771 231.036 86.4922 226.688 85.3203L226.18 40.2813C235.372 37.9375 244.122 36.362 252.43 35.5547L253.797 44.8516L235.086 47L235.477 58.6797L252.273 57.9766L251.648 67.1563L235.555 66.6094L235.242 78.2891Z"
            , SA.fill "#DCDCDC"
            ]
            []
        , Svg.path
            [ SA.d "M291.57 45.3984C288.523 45.5287 284.813 45.763 280.438 46.1016L280.789 62.3516L279.891 87L271.18 87.7031L270.711 46.4531C268.445 46.5052 266.349 46.5833 264.422 46.6875C262.521 46.7917 260.906 46.8438 259.578 46.8438L260.281 38.3672C269.292 37.1953 279.383 36.6094 290.555 36.6094L291.57 45.3984Z"
            , SA.fill "#DCDCDC"
            ]
            []
        , Svg.path
            [ SA.d "M29.5452 95.525L29.5573 95.5214C33.0735 94.4936 36.2343 92.5316 39.0382 89.7277C41.8216 86.9444 43.7698 83.7947 44.7966 80.2822C45.8181 76.7873 45.865 73.287 44.9144 69.8345C44.23 67.315 43.0641 64.9935 41.4478 62.8736L51.3366 72.7624L53.1044 74.5302L54.8722 72.7624L62.615 65.0196L64.3827 63.2518L62.615 61.4841L60.0395 58.9086C61.846 59.4948 63.6853 59.6919 65.5392 59.4349C68.6029 59.0102 71.3745 57.3914 73.858 54.908C75.1588 53.6071 76.1962 52.048 76.9896 50.2697L77.3683 49.4209L77.791 49.8436L79.5588 48.0758L90.13 37.5046C92.9558 34.6788 94.9647 31.5628 96.0484 28.1494L96.0517 28.1387C97.1297 24.6892 97.222 21.2347 96.2917 17.8335C95.3851 14.4085 93.5187 11.3362 90.8018 8.61928C88.179 5.99658 85.1568 4.22592 81.7493 3.39915C78.3767 2.5514 74.9581 2.69913 71.5483 3.7989C68.1441 4.88082 65.0528 6.86179 62.27 9.64458L51.6987 20.2158L49.931 21.9836L51.6987 23.7514L68.24 40.2927L66.9577 41.9696C66.3534 42.7598 65.9664 43.2127 65.7616 43.4175C64.9982 44.1809 64.5535 44.3253 64.4314 44.3456C64.3388 44.361 63.9064 44.3768 62.9799 43.8992C62.5971 43.6881 62.1997 43.4556 61.7877 43.2012C62.3178 41.3082 62.4821 39.4519 62.1818 37.6677C61.7749 35.2025 60.533 33.0271 58.6552 31.1492C56.8576 29.3517 54.6728 28.2657 52.1696 27.9605C49.7043 27.6598 47.2119 28.1412 44.7445 29.2494C42.2567 30.3193 39.9207 31.9939 37.7248 34.1898L27.0122 44.9024L25.2444 46.6702L27.0122 48.438L36.9405 58.3663C34.8062 56.7344 32.4839 55.5644 29.9748 54.8953C26.5042 53.9227 23.0013 53.9664 19.5228 55.0148C16.0281 56.0205 12.8811 57.9729 10.0822 60.7717C7.27963 63.5743 5.31814 66.7335 4.28994 70.2477C3.26348 73.7085 3.22333 77.2086 4.16924 80.695L4.17227 80.7061L4.17539 80.7172C5.15297 84.1931 7.05767 87.3043 9.79935 90.0459C12.5207 92.7672 15.6208 94.6581 19.0927 95.6345L19.1102 95.6395L19.1278 95.6441C22.5875 96.5631 26.0763 96.5199 29.5452 95.525ZM30.2548 76.3726L30.2561 76.3877L30.2577 76.4028C30.3946 77.7571 30.0196 78.806 29.068 79.7575C28.0831 80.7424 27.0185 81.1181 25.6779 80.9825C24.2914 80.8423 22.8076 80.1439 21.2191 78.5555C20.1703 77.5066 19.4892 76.4701 19.0936 75.4554L19.0896 75.4452L19.0855 75.4349C18.6692 74.3942 18.5876 73.5013 18.7304 72.7017L18.7349 72.6769L18.7388 72.6521C18.8681 71.8332 19.2102 71.1599 19.8049 70.5651C20.433 69.9371 21.1218 69.5908 21.9272 69.4637L21.9521 69.4597L21.9768 69.4553C22.7765 69.3125 23.6693 69.3941 24.7101 69.8104L24.7438 69.8239L24.7779 69.8364C25.7973 70.2102 26.8294 70.8721 27.8659 71.9087C29.4444 73.4871 30.1302 74.9711 30.2548 76.3726ZM48.125 41.9703L48.1603 41.9625L48.1953 41.9538C48.1971 41.9533 48.1988 41.9529 48.2004 41.9525C48.2375 41.9729 48.3177 42.0249 48.4375 42.1447C48.6817 42.3889 48.7342 42.5348 48.7446 42.5702L48.7451 42.5718C48.7507 42.5905 48.7656 42.6403 48.7285 42.7742C48.5969 43.187 48.0542 44.0858 46.6654 45.5345L45.3262 44.1953L45.998 43.5236C47.1713 42.3503 47.8709 42.026 48.125 41.9703ZM81.5238 24.2605L81.5243 24.2658C81.6284 25.3852 81.2502 26.5854 79.8769 27.9586L79.2406 28.595L71.1795 20.534L71.8159 19.8976C73.3063 18.4073 74.5085 18.0195 75.4803 18.0795L75.511 18.0814L75.5418 18.0826C76.67 18.1244 77.9058 18.6336 79.2759 20.0037C80.7466 21.4743 81.399 22.8874 81.5238 24.2605Z"
            , SA.fill "#28BA62"
            , SA.stroke "#141414"
            , SA.strokeWidth "5"
            ]
            []
        , Svg.path
            [ SA.d "M317 76.5C317 82.299 312.299 87 306.5 87C300.701 87 296 82.299 296 76.5C296 70.701 300.701 66 306.5 66C312.299 66 317 70.701 317 76.5Z"
            , SA.fill "#141414"
            , SA.stroke "#28BA62"
            , SA.strokeWidth "6"
            ]
            []
        , Svg.node "script"
            []
            []
        ]


close : Html Msg
close =
    Svg.svg
        [ SA.viewBox "6.29 6.34 11.31 11.31"
        , SA.fill "currentColor"
        ]
        [ Svg.path
            [ SA.d "m16.192 6.344-4.243 4.242-4.242-4.242-1.414 1.414L10.535 12l-4.242 4.242 1.414 1.414 4.242-4.242 4.243 4.242 1.414-1.414L13.364 12l4.242-4.242z"
            ]
            []
        ]
    


subscriptions : Model -> Sub Msg
subscriptions _ =
    Port.receiveFavorites Port_GotFavorites


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
