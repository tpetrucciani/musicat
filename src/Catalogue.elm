module Catalogue exposing
    ( Album
    , Artist
    , ArtistId
    , BookletPath
    , Catalogue
    , Config
    , CoverPath
    , Entry
    , Genre
    , GenreId
    , Grouping
    , QobuzId
    , SpotifyId
    , artistMatchesFilter
    , catalogueDecoder
    )

import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import String.Normalize
import Tuple



-- TYPE DEFINITIONS


type alias Catalogue =
    { genres : List Genre
    , artists : List Artist
    , albums : List Album
    , config : Config
    }


type alias GenreId =
    String


type alias Genre =
    { id : GenreId
    , name : String
    , sortKey : String
    }


type alias ArtistId =
    String


type alias Artist =
    { id : ArtistId
    , name : String
    , shortName : String
    , sortKey : String
    }


type alias Album =
    { cover : CoverPath
    , entries : List Entry
    , qobuz : Maybe QobuzId
    , spotify : Maybe SpotifyId
    , local : Bool
    , archived : Bool
    , booklet : Maybe BookletPath
    }


type alias CoverPath =
    String


type alias Entry =
    { genre : Genre
    , artist : Artist
    , grouping : Maybe Grouping
    }


type alias QobuzId =
    String


type alias SpotifyId =
    String


type alias BookletPath =
    String


type alias Grouping =
    { name : String
    , sortKey : String
    }


type alias Config =
    { selectedGenre : GenreId }



-- DECODER: HELPER FUNCTIONS


pair : Decoder a -> Decoder b -> Decoder ( a, b )
pair da db =
    D.map2 Tuple.pair da db


optionalField : String -> Decoder a -> Decoder (Maybe a)
optionalField fieldName fieldDecoder =
    D.maybe (D.field fieldName fieldDecoder)


optionalFieldWithDefault : String -> Decoder a -> a -> Decoder a
optionalFieldWithDefault fieldName fieldDecoder default =
    D.oneOf [ D.field fieldName fieldDecoder, D.succeed default ]


stringField : String -> Decoder String
stringField fieldName =
    D.field fieldName D.string


optionalStringField : String -> Decoder (Maybe String)
optionalStringField fieldName =
    optionalField fieldName D.string


optionalStringFieldWithDefault : String -> String -> Decoder String
optionalStringFieldWithDefault fieldName default =
    optionalFieldWithDefault fieldName D.string default



-- DECODER: CATALOGUE DECODER


catalogueDecoder : Decoder Catalogue
catalogueDecoder =
    pair (D.field "genres" genresDecoder) (D.field "artists" artistsDecoder)
        |> D.andThen
            (\( g, a ) ->
                D.map4 Catalogue
                    (g |> Dict.values |> D.succeed)
                    (a |> Dict.values |> D.succeed)
                    (D.field "albums" (D.list (albumDecoder g a)))
                    (D.field "config" (configDecoder g))
            )


type alias GenreInfo =
    { name : String, sortKey : String }


genresDecoder : Decoder (Dict GenreId Genre)
genresDecoder =
    let
        genreInfoDecoder : Decoder GenreInfo
        genreInfoDecoder =
            stringField "name"
                |> D.andThen
                    (\n ->
                        D.map2 GenreInfo
                            (D.succeed n)
                            (D.map simplifyString
                                (optionalStringFieldWithDefault "sortKey" n)
                            )
                    )

        genreInfoToGenre : GenreId -> GenreInfo -> Genre
        genreInfoToGenre id { name, sortKey } =
            Genre id name sortKey
    in
    D.map (Dict.map genreInfoToGenre) (D.dict genreInfoDecoder)


type alias ArtistInfo =
    { name : String, shortName : String, sortKey : String }


artistsDecoder : Decoder (Dict ArtistId Artist)
artistsDecoder =
    let
        artistInfoDecoder : Decoder ArtistInfo
        artistInfoDecoder =
            stringField "name"
                |> D.andThen
                    (\n ->
                        D.map3 ArtistInfo
                            (D.succeed n)
                            (optionalStringFieldWithDefault "shortName" n)
                            (D.map simplifyString
                                (optionalStringFieldWithDefault "sortKey" n)
                            )
                    )

        artistInfoToArtist : ArtistId -> ArtistInfo -> Artist
        artistInfoToArtist id { name, shortName, sortKey } =
            Artist id name shortName sortKey
    in
    D.map (Dict.map artistInfoToArtist) (D.dict artistInfoDecoder)


albumDecoder : Dict GenreId Genre -> Dict ArtistId Artist -> Decoder Album
albumDecoder genres artists =
    D.map7 Album
        (stringField "cover")
        (D.field "entries" (D.list (entryDecoder genres artists)))
        (optionalStringField "qobuz")
        (optionalStringField "spotify")
        (optionalFieldWithDefault "local" D.bool False)
        (optionalFieldWithDefault "archived" D.bool False)
        (optionalStringField "booklet")


stringFieldWithLookupInDict : String -> Dict String a -> Decoder a
stringFieldWithLookupInDict fieldName dict =
    stringField fieldName
        |> D.andThen
            (\id ->
                Dict.get id dict
                    |> Maybe.map D.succeed
                    |> Maybe.withDefault
                        (D.fail ("Identifier " ++ id ++ " not found"))
            )


entryDecoder : Dict GenreId Genre -> Dict ArtistId Artist -> Decoder Entry
entryDecoder genres artists =
    D.map3 Entry
        (stringFieldWithLookupInDict "genre" genres)
        (stringFieldWithLookupInDict "artist" artists)
        groupingDecoder


groupingDecoder : Decoder (Maybe Grouping)
groupingDecoder =
    let
        actualGroupingDecoder : Decoder Grouping
        actualGroupingDecoder =
            D.andThen
                (\g ->
                    D.map2 Grouping
                        (D.succeed g)
                        (D.map simplifyString
                            (optionalStringFieldWithDefault "groupingSortKey" g)
                        )
                )
                (stringField "grouping")
    in
    D.oneOf [ D.map Just actualGroupingDecoder, D.succeed Nothing ]


configDecoder : Dict GenreId Genre -> Decoder Config
configDecoder genres =
    D.map (.id >> Config) (stringFieldWithLookupInDict "selectedGenre" genres)



-- STRING SIMPLIFICATION AND COMPARISON


simplifyString : String -> String
simplifyString =
    String.toLower >> String.Normalize.removeDiacritics


compareBySortKey : { a | sortKey : String } -> { b | sortKey : String } -> Order
compareBySortKey x y =
    compare x.sortKey y.sortKey


compareGenres : Genre -> Genre -> Order
compareGenres =
    compareBySortKey


compareArtists : Artist -> Artist -> Order
compareArtists =
    compareBySortKey


artistMatchesFilter : String -> Artist -> Bool
artistMatchesFilter filter artist =
    let
        nameWords =
            artist.name |> simplifyString |> String.words
    in
    List.all
        (\w -> List.any (String.startsWith w) nameWords)
        (String.words filter)
