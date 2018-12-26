module Main exposing (Model(..), Msg(..), init, main, subscriptions, update, view)

import Browser
import Html exposing (Html, a, div, img, text)
import Html.Attributes exposing (href, src)
import Http
import Json.Decode exposing (Decoder, field, list, map, map2, map4, string)



-- MAIN


main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type Model
    = Failure String
    | Loading
    | Success Catalogue


type alias Catalogue =
    { albums : List Album }


type alias Album =
    { cover : CoverPath
    , spotify : SpotifyId
    , entries : List Entry
    , comment : CommentString
    }


type alias Entry =
    { genre : Genre
    , artist : Artist
    }


type alias CoverPath =
    String


type alias SpotifyId =
    String


type alias Genre =
    String


type alias Artist =
    String


type alias CommentString =
    String


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading
    , Http.get
        { url = "/_data/catalogue"
        , expect = Http.expectJson GotCatalogue catalogueDecoder
        }
    )


catalogueDecoder : Decoder Catalogue
catalogueDecoder =
    map Catalogue (field "albums" (list albumDecoder))


albumDecoder : Decoder Album
albumDecoder =
    map4 Album
        (field "cover" string)
        (field "spotify" string)
        (field "entries" (list entryDecoder))
        (field "comment" string)


entryDecoder : Decoder Entry
entryDecoder =
    map2 Entry (field "genre" string) (field "artist" string)



-- UPDATE


type Msg
    = GotCatalogue (Result Http.Error Catalogue)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotCatalogue result ->
            case result of
                Ok catalogue ->
                    ( Success catalogue, Cmd.none )

                Err (Http.BadBody err) ->
                    ( Failure err, Cmd.none )

                Err _ ->
                    ( Failure "Error", Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Album catalogue"
    , body = [ viewBody model ]
    }


viewBody : Model -> Html msg
viewBody model =
    case model of
        Failure err ->
            text err

        Loading ->
            text "Loading..."

        Success catalogue ->
            div []
                (List.map displayAlbum catalogue.albums)


displayAlbum : Album -> Html msg
displayAlbum album =
    div []
        [ a [ href ("spotify:album:" ++ album.spotify) ]
            [ img [ src ("_data/covers/" ++ album.cover) ] [] ]
        ]
