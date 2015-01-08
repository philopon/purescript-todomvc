module Main where

import Control.Monad
import Control.Monad.Eff
import qualified Data.String as String
import Data.Array(filter, null, length)
import Data.Foldable(all)
import qualified FRP.Kefir as K
import Data.Html
import qualified Data.Html.Attributes.Html5 as A
import qualified Data.Html.Elements.Html5 as E
import qualified Data.Html.Events as EV
import qualified Data.Html.Lazy as L
import DOM

newtype TaskId = TaskId Number
instance eqTaskId :: Eq TaskId where
  (==) (TaskId a) (TaskId b) = a == b
  (/=) a b = not $ a == b

succTaskId :: TaskId -> TaskId
succTaskId (TaskId i) = TaskId (i + 1)

unTaskId :: TaskId -> Number
unTaskId (TaskId i) = i

data Visibility
  = All
  | Completed
  | Active

instance eqVisibility :: Eq Visibility where
  (==) All All = true
  (==) Completed Completed = true
  (==) Active Active = true
  (==) _ _ = false
  (/=) a b = not (a == b)

instance showVisibility :: Show Visibility where
  show All = "All"
  show Completed = "Completed"
  show Active = "Active"

type State =
  { tasks      :: [Task]
  , field      :: String
  , uid        :: TaskId
  , visibility :: Visibility
  }

type Task =
  { description :: String
  , completed   :: Boolean
  , editing     :: Boolean
  , id          :: TaskId
  }

newTask :: String -> TaskId -> Task
newTask desc id =
  { description: desc
  , completed:   false
  , editing:     false
  , id:          id
  }

emptyState :: State
emptyState =
  { tasks:      []
  , visibility: All
  , field:      ""
  , uid:        TaskId 0
  }

data Update
  = UpdateField String
  | EditingTask TaskId Boolean
  | UpdateTask  TaskId String
  | Add
  | Delete TaskId
  | DeleteComplete
  | Check TaskId Boolean
  | CheckAll Boolean
  | ChangeVisibility Visibility

step :: State -> Update -> State
step state update = case update of
  Add  -> state { uid = succTaskId state.uid
                , field = ""
                , tasks = if String.null state.field
                             then state.tasks
                             else state.tasks ++ [newTask state.field state.uid]
                }
  UpdateField str    -> state { field = str }
  EditingTask id isEditing ->
    let stepTask t = if t.id == id then t { editing = isEditing } else t
     in state { tasks = stepTask <$> state.tasks }
  UpdateTask id task ->
    let stepTask t = if t.id == id then t { description = task } else t
     in state { tasks = stepTask <$> state.tasks }
  Delete id -> state { tasks = filter (\t -> t.id /= id) state.tasks }
  DeleteComplete ->
    state { tasks = filter (\t -> not t.completed) state.tasks }
  Check id isCompleted ->
    let stepTask t = if t.id == id then t { completed = isCompleted } else t
     in state { tasks = stepTask <$> state.tasks }
  CheckAll isCompleted ->
    let stepTask t = t { completed = isCompleted }
     in state { tasks = stepTask <$> state.tasks }
  ChangeVisibility visibility ->
    state { visibility = visibility }

view :: State -> VTree
view state =
  E.div [ A.class_ "todomvc-wrapper" ]
    [ E.section [ A.id_ "todoapp" ]
        [ taskEntry state.field
        , taskList state.visibility state.tasks
        , controls state.visibility state.tasks
        ]
    , infoFooter
    ]

onEnter f = EV.onKeyDown $ \k -> when (EV.keyCode k == 13) f

taskEntry :: String -> VTree
taskEntry task =
  E.header
    [ A.id_ "header" ]
    [ E.h1 [] [E.text "todos"]
    , E.input
      [ A.id_ "new-todo"
      , A.placeholder "What needs to be done?"
      , A.autofocus true
      , A.value task
      , A.name "newTodo"
      , EV.onInput (\v -> K.emitAsync updates $ UpdateField $ EV.targetValue v)
      , onEnter (K.emitAsync updates Add)
      ]
      []
    ]

taskList :: Visibility -> [Task] -> VTree
taskList visibility tasks =
  let isVisible todo = case visibility of
        Completed -> todo.completed
        Active    -> not todo.completed
        All       -> true

      allCompleted = all (\t -> t.completed) tasks

      cssVisibility = if null tasks then "hidden" else "visible"

   in E.section
        [ A.id_ "main"
        , A.style {visibility: cssVisibility}
        ]
        [ E.input
            [ A.id_ "toggle-all"
            , A.type_ "checkbox"
            , A.name "toggle"
            , A.checked allCompleted
            , EV.onClick (\_ -> K.emitAsync updates (CheckAll (not allCompleted)))
            ] []
        , E.label
            [ A.for "toggle-all" ]
            [ E.text "Mark all as complete" ]
        , E.ul
            [ A.id_ "todo-list" ]
            (todoItem <$> filter isVisible tasks)
        ]

todoItem :: Task -> VTree
todoItem todo =
  let className = (if todo.completed then "completed " else "") ++
                  (if todo.editing   then "editing"    else "")
   in E.li
        [ A.class_ className ]
        [ E.div
            [ A.class_ "view" ]
            [ E.input
                [ A.class_ "toggle"
                , A.type_ "checkbox"
                , A.checked todo.completed
                , EV.onClick (\_ -> K.emitAsync updates (Check todo.id (not todo.completed)))
                ] []
            , E.label
                [ EV.onDoubleClick (\_ -> K.emitAsync updates (EditingTask todo.id true)) ]
                [ E.text todo.description ]
            , E.button
                [ A.class_ "destroy"
                , EV.onClick (\_ -> K.emitAsync updates (Delete todo.id))
                ] []
            ]
        , E.input
            [ A.class_ "edit"
            , A.value todo.description
            , A.name "title"
            , A.id_ ("todo-" ++ show (unTaskId todo.id))
            , EV.onInput (\v -> K.emitAsync updates $ UpdateTask todo.id $ EV.targetValue v)
            , EV.onBlur  (\_ -> K.emitAsync updates $ EditingTask todo.id false)
            , onEnter    (K.emitAsync updates $ EditingTask todo.id false)
            ] []
        ]

controls :: Visibility -> [Task] -> VTree
controls visibility tasks =
  let tasksCompleted = length (filter (\t -> t.completed) tasks)
      tasksLeft = length tasks - tasksCompleted
      item_ = if tasksLeft == 1 then " item" else " items"
   in E.footer
        [ A.id_ "footer"
        , A.hidden (null tasks)
        ]
        [ E.span
            [ A.id_ "todo-count" ]
            [ E.strong [] [E.text $ show tasksLeft]
            , E.text (item_ ++ " left")
            ]
        , E.ul
            [ A.id_ "filters" ]
            [ visibilitySwap "#/" All visibility
            , E.text " "
            , visibilitySwap "#/active" Active visibility
            , E.text " "
            , visibilitySwap "#/completed" Completed visibility
            ]
        , E.button
            [ A.class_ "clear-completed"
            , A.id_ "clear-completed"
            , A.hidden (tasksCompleted == 0)
            , EV.onClick (\_ -> K.emitAsync updates DeleteComplete)
            ]
            [ E.text $ "Clear completed (" ++ show tasksCompleted ++ ")" ]
        ]

visibilitySwap uri visibility actualVisibility =
  let className = if visibility == actualVisibility then "selected" else ""
   in E.li
        [ EV.onClick (\_ -> K.emitAsync updates (ChangeVisibility visibility)) ]
        [ E.a [ A.class_ className, A.href uri] [ E.text $ show visibility ]]

infoFooter :: VTree
infoFooter = E.footer [ A.id_ "info" ]
  [ E.p [] [ E.text "Double-click to edit a todo" ]
  , E.p [] [ E.text "Original on "
           , E.a [A.href "https://github.com/evancz/elm-todomvc"] [E.text "elm-todomvc"]
           ]
  ]

foreign import appendBody """
function appendBody(node) {
  return function (){
    document.body.appendChild(node);
  }
}""" :: forall e. Node -> Eff (dom :: DOM | e) Unit

main = do
  html <- createElement (E.text "loading...")
  getNode html >>= appendBody
  K.onValue state $ \v -> patch (view v) html

state :: K.Property () K.OT State
state = K.unsafeGlobalize $ K.scan step startingState updates

startingState :: State
startingState = emptyState

updates :: K.Stream K.E K.OT Update
updates = K.unsafeGlobalize K.emitter
