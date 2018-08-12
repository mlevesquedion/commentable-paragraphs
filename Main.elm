module Main exposing (..)

import Css exposing (..)
import Css.Transitions exposing (easeInOut, transition)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, value)
import Html.Styled.Events exposing (onClick, onInput)


posts : List Post
posts =
    [ { id = 1
      , title = "Cats are awesome"
      , paragraphs =
            [ { id = 1
              , content = "The domestic cat (Felis silvestris catus or Felis catus) is a small, typically furry, carnivorous mammal. They are often called house cats when kept as indoor pets or simply cats when there is no need to distinguish them from other felids and felines. They are often valued by humans for companionship and for their ability to hunt vermin."
              , comments = []
              , currentComment = "Personally, I..."
              , open = True
              }
            , { id = 2
              , content = "There are more than seventy cat breeds recognized by various cat registries."
              , comments =
                    [ { author = "Mr. Cat"
                      , content = "I have to disagree. I believe there are at least three."
                      }
                    ]
              , currentComment = ""
              , open = False
              }
            , { id = 3
              , content = "Thanks to Wikipedia for this article: https://en.wikipedia.org/wiki/Cat"
              , comments = []
              , currentComment = ""
              , open = False
              }
            ]
      }
    , { id = 2
      , title = "My first post"
      , paragraphs =
            [ { id = 1
              , content = "Hey guys. This is my first post. Just checking out this new on-paragraph comments feature."
              , comments =
                    [ { author = "Me"
                      , content = "This is pretty neat!"
                      }
                    , { author = "Me"
                      , content = "Yep, very neat indeed."
                      }
                    ]
              , currentComment = ""
              , open = False
              }
            , { id = 2
              , content = "This is a paragraph. Neat."
              , comments =
                    [ { author = "A"
                      , content = "1"
                      }
                    , { author = "B"
                      , content = "2"
                      }
                    , { author = "C"
                      , content = "3"
                      }
                    , { author = "This is"
                      , content = "just to show that the highlighting increases based on the number of comments."
                      }
                    ]
              , currentComment = ""
              , open = False
              }
            ]
      }
    ]


type alias Model =
    { commenterName : String
    , posts : List Post
    , showComments : Bool
    }


type alias Post =
    { id : ParagraphId
    , title : String
    , paragraphs : List Paragraph
    }


type alias Paragraph =
    { id : ParagraphId
    , content : String
    , comments : List Comment
    , currentComment : String
    , open : Bool
    }


type alias Comment =
    { author : String
    , content : String
    }


init : Model
init =
    { commenterName = ""
    , posts = posts
    , showComments = True
    }


type alias PostId =
    Int


type alias ParagraphId =
    Int


type Msg
    = UpdateCommenterName String
    | UpdateCurrentComment PostId ParagraphId String
    | CreateComment PostId ParagraphId String String
    | ToggleComments PostId ParagraphId
    | ToggleAllComments


view : Model -> Html Msg
view model =
    div []
        ([ div
            [ css
                [ displayFlex
                , justifyContent spaceBetween
                ]
            ]
            [ h1 [] [ text "My awesome blog" ]
            , button [ onClick ToggleAllComments ] [ text "Toggle comments" ]
            ]
         ]
            ++ (List.map (viewPost model.showComments model.commenterName) model.posts)
        )


viewPost : Bool -> String -> Post -> Html Msg
viewPost showComments commenterName post =
    div []
        ([ h2 [] [ text post.title ] ]
            ++ (List.map (viewParagraph showComments commenterName post.id) post.paragraphs)
        )


maxHighlighting : Int
maxHighlighting =
    5


highlighting : Int -> Color
highlighting level =
    let
        transparency =
            min level maxHighlighting
                |> toFloat
    in
        (rgba 255 255 0 (transparency * 0.2))


viewParagraph : Bool -> String -> PostId -> Paragraph -> Html Msg
viewParagraph showComments commenterName postId paragraph =
    if showComments then
        div []
            [ p
                [ css
                    [ backgroundColor (highlighting (List.length paragraph.comments))
                    , hover
                        [ backgroundColor (highlighting maxHighlighting)
                        , cursor grab
                        ]
                    ]
                , onClick (ToggleComments postId paragraph.id)
                ]
                [ text paragraph.content ]
            , if paragraph.open then
                div
                    []
                    [ (viewComments paragraph.comments)
                    , (viewCurrentComment postId paragraph.id commenterName paragraph.currentComment)
                    ]
              else
                text ""
            ]
    else
        div []
            [ p []
                [ text paragraph.content ]
            ]


viewComments : List Comment -> Html Msg
viewComments comments =
    ul []
        (List.map viewComment comments)


viewComment : Comment -> Html Msg
viewComment comment =
    li []
        [ h5 [] [ text comment.author ]
        , span [] [ text comment.content ]
        ]


viewCurrentComment : PostId -> ParagraphId -> String -> String -> Html Msg
viewCurrentComment postId paragraphId author content =
    div []
        [ div []
            [ text "Your name: "
            , input [ value author, onInput UpdateCommenterName ] []
            , text "Your two cents: "
            , input [ value content, onInput (UpdateCurrentComment postId paragraphId) ] []
            ]
        , button [ onClick (CreateComment postId paragraphId author content) ] [ text "Submit" ]
        ]


updateParagraph : List Post -> PostId -> ParagraphId -> (Paragraph -> Paragraph) -> List Post
updateParagraph posts postId paragraphId func =
    List.map
        (\post ->
            let
                newParagraphs =
                    if post.id == postId then
                        (List.map
                            (\paragraph ->
                                if paragraph.id == paragraphId then
                                    func paragraph
                                else
                                    paragraph
                            )
                        )
                            post.paragraphs
                    else
                        post.paragraphs
            in
                { post | paragraphs = newParagraphs }
        )
        posts


update : Msg -> Model -> Model
update msg model =
    case msg of
        ToggleAllComments ->
            { model | showComments = not model.showComments }

        ToggleComments postId paragraphId ->
            let
                newPosts =
                    updateParagraph model.posts
                        postId
                        paragraphId
                        (\paragraph -> { paragraph | open = not paragraph.open })
            in
                { model | posts = newPosts }

        UpdateCurrentComment postId paragraphId newContent ->
            let
                newPosts =
                    updateParagraph model.posts
                        postId
                        paragraphId
                        (\paragraph ->
                            { paragraph
                                | currentComment = newContent
                            }
                        )
            in
                { model | posts = newPosts }

        CreateComment postId paragraphId author content ->
            let
                newPosts =
                    updateParagraph model.posts
                        postId
                        paragraphId
                        (\paragraph ->
                            { paragraph
                                | comments = paragraph.comments ++ [ { author = author, content = content } ]
                                , currentComment = ""
                            }
                        )
            in
                { model | posts = newPosts }

        UpdateCommenterName newName ->
            { model | commenterName = newName }


main : Program Never Model Msg
main =
    beginnerProgram
        { model = init
        , view = view
        , update = update
        }
