module Catalogue exposing (Album, Artist, ArtistInfo, BookletPath, Catalogue, Config, CoverPath, Entry, Genre, GenreInfo, QobuzId, SpotifyId, catalogueDecoder)

import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder, bool, field, list, map, map2, map8, maybe, null, oneOf, string, succeed)



-- TYPE DEFINITIONS


type alias Catalogue =
    { config : Config
    , genres : Dict Genre GenreInfo
    , artists : Dict Artist ArtistInfo
    , albums : List Album
    }


type alias Config =
    { selectedGenre : Genre }


type alias Genre =
    String


type alias GenreInfo =
    { name : String
    , sort : String
    }


type alias Artist =
    String


type alias ArtistInfo =
    { name : String
    , shortName : String
    , sort : String
    }


type alias Album =
    { cover : CoverPath
    , entries : List Entry
    , spotify : Maybe SpotifyId
    , qobuz : Maybe QobuzId
    , local : Bool
    , archived : Bool
    , booklet : Maybe BookletPath
    }


type alias Entry =
    { genre : Genre
    , artist : Artist
    }


type alias CoverPath =
    String


type alias BookletPath =
    String


type alias SpotifyId =
    String


type alias QobuzId =
    String



-- DECODER


catalogueDecoder : Decoder Catalogue
catalogueDecoder =
    D.map4 Catalogue
        (field "config" configDecoder)
        (field "genres" genresDecoder)
        (field "artists" artistsDecoder)
        (field "albums" (list albumDecoder))


configDecoder : Decoder Config
configDecoder =
    map Config (field "selectedGenre" string)


genresDecoder : Decoder (Dict Genre GenreInfo)
genresDecoder =
    let
        genreInfoDecoder =
            field "name" string
                |> D.andThen
                    (\n ->
                        D.map2 GenreInfo
                            (succeed n)
                            (optionalField "sort" string n)
                    )
    in
    D.dict genreInfoDecoder


artistsDecoder : Decoder (Dict Artist ArtistInfo)
artistsDecoder =
    let
        artistInfoDecoder =
            field "name" string
                |> D.andThen
                    (\n ->
                        D.map3 ArtistInfo
                            (succeed n)
                            (optionalField "shortName" string n)
                            (optionalField "sort" string n)
                    )
    in
    D.dict artistInfoDecoder


optionalField : String -> Decoder a -> a -> Decoder a
optionalField fieldName fieldDecoder default =
    oneOf [ field fieldName fieldDecoder, succeed default ]


albumDecoder : Decoder Album
albumDecoder =
    D.map7 Album
        (field "cover" string)
        (field "entries" (list entryDecoder))
        (maybe (field "spotify" string))
        (maybe (field "qobuz" string))
        (optionalField "local" bool False)
        (optionalField "archived" bool False)
        (maybe (field "booklet" string))


entryDecoder : Decoder Entry
entryDecoder =
    map2 Entry (field "genre" string) (field "artist" string)
