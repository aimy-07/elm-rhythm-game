module Page.Home.RankingRecords.RankingRecord exposing
    ( RankingRecord
    , convetUidToUser
    , isOwnRecord
    , new
    , toFirst
    , toSecond
    , toThird
    )

import PublicRecord exposing (PublicRecord)
import Session.User exposing (User)
import Session.User.Uid exposing (Uid)


type RankingRecord
    = RankingRecord
        { first : Maybe BestRecord
        , second : Maybe BestRecord
        , third : Maybe BestRecord
        }


type alias BestRecord =
    { user : User
    , score : Int
    }


toFirst : RankingRecord -> Maybe BestRecord
toFirst (RankingRecord { first }) =
    first


toSecond : RankingRecord -> Maybe BestRecord
toSecond (RankingRecord { second }) =
    second


toThird : RankingRecord -> Maybe BestRecord
toThird (RankingRecord { third }) =
    third


new : List User -> PublicRecord -> RankingRecord
new users publicRecord =
    let
        first =
            publicRecord
                |> PublicRecord.toBestRecords
                |> PublicRecord.sortBestRecords
                |> List.head
                |> convetUidToUser users

        second =
            publicRecord
                |> PublicRecord.toBestRecords
                |> PublicRecord.sortBestRecords
                |> List.drop 1
                |> List.head
                |> convetUidToUser users

        third =
            publicRecord
                |> PublicRecord.toBestRecords
                |> PublicRecord.sortBestRecords
                |> List.drop 2
                |> List.head
                |> convetUidToUser users
    in
    RankingRecord
        { first = first
        , second = second
        , third = third
        }


convetUidToUser : List User -> Maybe PublicRecord.BestRecord -> Maybe BestRecord
convetUidToUser users maybeBestRecord =
    maybeBestRecord
        |> Maybe.andThen
            (\{ uid, score } ->
                users
                    |> List.filter (.uid >> (==) uid)
                    |> List.head
                    |> Maybe.map
                        (\user ->
                            { user = user
                            , score = score
                            }
                        )
            )


isOwnRecord : Maybe BestRecord -> Uid -> Bool
isOwnRecord maybeBestRecord uid =
    maybeBestRecord
        |> Maybe.map (.user >> .uid >> (==) uid)
        |> Maybe.withDefault False
