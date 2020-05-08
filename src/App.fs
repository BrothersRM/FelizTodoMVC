module App

open Browser.Types
open System
open Feliz
open Feliz.Bulma
open Feliz.Bulma.Checkradio
open Elmish
open Fable.Core
open Feliz
open Feliz.prop
open Feliz.style
open Zanaptak.TypedCssClasses
open FSharp.Data.JsonProvider

type todocss = CssClasses<"../node_modules/todomvc-app-css/index.css", Naming.PascalCase>
type jsonexample = JsonProvider<"example.json">

type Todo = {
    ID: int
    Completed: bool
    Text: string 
}

type State = {
    NewTodoText: string
    Todos: Todo list
    CurrentlyEditing: Todo option
    EditText: string
}

let getNewTodoID (todos: Todo list) =
    if todos.Length > 0 then
        (todos |> List.maxBy (fun x -> x.ID)).ID + 1
    else
        0

type Msg =
    | UpdateNewTodoText of string
    | AddTodo of Todo
    | UpdateTodo of Todo
    | ToggleAllTodos of bool
    | ClearCompleted
    | DeleteTodo of Todo
    | StartEditing of Todo
    | StopEditing
    | UpdateEditText of string
    
let init() = {NewTodoText = ""; Todos = [ ]; CurrentlyEditing = None; EditText = ""}, Cmd.none

let update (msg: Msg) (state: State) =
    match msg with
    | UpdateNewTodoText text -> {state with NewTodoText = text}, Cmd.none 
    | AddTodo todo ->
        match state.NewTodoText with
        | "" -> state, Cmd.none 
        | _ -> {state with Todos = todo::state.Todos; NewTodoText = ""}, Cmd.none
    | UpdateTodo todo ->
        let newtodos = state.Todos |> List.map (fun x -> if todo.ID = x.ID then todo else x)
        {state with Todos = newtodos}, Cmd.none
    | ToggleAllTodos v ->
        {state with Todos = state.Todos |> List.map (fun x -> {x with Completed = v})}, Cmd.none
    | ClearCompleted ->
        {state with Todos = state.Todos |> List.filter (fun x -> x.Completed = false)}, Cmd.none
    | DeleteTodo todo ->
        {state with Todos = state.Todos |> List.filter (fun x -> x <> todo)}, Cmd.none
    | StartEditing todo ->
        {state with CurrentlyEditing = Some todo; EditText = todo.Text}, Cmd.none
    | StopEditing ->
        match state.CurrentlyEditing with
        | Some todo ->
            let newTodo = {todo with Text = state.EditText}
            {state with EditText = ""; CurrentlyEditing = None}, Cmd.ofMsg (UpdateTodo newTodo)
        | _ -> state, Cmd.none
    | UpdateEditText str ->
        {state with EditText = str}, Cmd.none

let activeTodos state dispatch =
    seq {
        yield! state.Todos |> List.sortBy (fun item -> item.ID) |> List.map (fun todo -> 
            let editing =
                match state.CurrentlyEditing with
                | Some t when t = todo -> true
                | _ -> false
            Html.li [
                prop.className [
                    if todo.Completed then todocss.Completed else ""
                    if editing then todocss.Editing else ""
                ]
                prop.children [
                    Html.div [
                        prop.className todocss.View
                        prop.style [
                            display.block
                        ]
                        prop.children [
                            Html.div [
                                prop.style [
                                    if editing then display.none else display.initial
                                ]
                                prop.children [
                                    Html.input [
                                        prop.className todocss.Toggle
                                        prop.onChange (fun x -> dispatch (UpdateTodo {todo with Completed = x}) )
                                        type'.checkbox
                                        prop.isChecked todo.Completed
                                    ]
                                    Html.label [
                                        prop.onDoubleClick (fun x ->
                                            dispatch (StartEditing todo)
                                        ) 
                                        prop.text todo.Text
                                    ]
                                    Html.button [
                                        prop.className todocss.Destroy
                                        prop.onClick (fun _ -> dispatch (DeleteTodo todo))
                                    ]
                                ]
                            ]
                            Html.input [
                                prop.className todocss.Edit
                                prop.value state.EditText
                                prop.onChange (fun x -> dispatch (UpdateEditText x))
                                prop.onBlur (fun x -> dispatch StopEditing)
                                prop.style [
                                    if editing then display.block else display.none
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        )
    }

let mainContent state dispatch=
    Html.div [
            Html.section [
                prop.className [
                    todocss.Todoapp
                ]
                prop.children [
                    Html.header [
                        Html.h1 "todos"
                        Html.input [
                            prop.className [
                                todocss.NewTodo
                            ]
                            prop.placeholder "What needs to be done?"
                            prop.value state.NewTodoText
                            prop.onChange (fun x -> dispatch (UpdateNewTodoText x))
                            prop.onKeyDown (key.enter, fun x -> dispatch (AddTodo {ID= getNewTodoID state.Todos  ; Completed=false; Text=state.NewTodoText}))
                        ]
                    ]
                    Html.section [
                        prop.className [
                            todocss.Main
                        ]
                        prop.children [
                            Html.input [
                                prop.className todocss.ToggleAll
                                type'.checkbox
                                prop.onChange (fun x -> dispatch (ToggleAllTodos x))
                                prop.id "toggle-all"
                            ]
                            Html.label [
                                prop.htmlFor "toggle-all"
                            ]
                            Html.ul [
                                prop.className todocss.TodoList
                                prop.children [
                                    yield! activeTodos state dispatch
                                ]
                            ]
                        ]
                    ]
                    Html.footer [
                        prop.className todocss.Footer
                        prop.children [
                            Html.span [
                                prop.className todocss.TodoCount
                                prop.children [
                                     if state.Todos.Length = 0 then
                                         Html.span "Nothing left to do!"
                                     else if state.Todos.Length = 1 then
                                         Html.span "1 item left"
                                     else
                                         Html.span (sprintf "%i items left" state.Todos.Length)                               
                                ]
                            ]
                            Html.ul [
                                prop.className todocss.Filters
                                prop.children [
                                    Html.li [
                                        Html.a [
                                            prop.text "All"
                                            prop.href "#"
                                        ]
                                        Html.a [
                                            prop.text "Active"
                                            prop.href "#/active"
                                        ]
                                        Html.a [
                                            prop.text "Completed"
                                            prop.href "#/completed"
                                        ]
                                    ]
                                ]
                            ]
                            if (state.Todos |> List.filter (fun x -> x.Completed = true)).Length > 0 then 
                                Html.button [
                                    prop.className  todocss.ClearCompleted
                                    prop.text "Clear completed"
                                    prop.onClick (fun _ -> dispatch ClearCompleted)
                                    prop.style [
                                        //display.block
                                    ]
                                ]
                            else
                                Html.none
                        ]
                    ]
                ]
            ]     
        ]   

let render (state: State) (dispatch: Msg -> unit) =
    mainContent state dispatch
    
