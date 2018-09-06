namespace Samples

open WebSharper
open WebSharper.JavaScript
open WebSharper.UI
open WebSharper.UI.Html
open WebSharper.UI.Client
open WebSharper.Forms


[<JavaScript>]
module PersonPets =

    type Species =
        | Cat | Dog | Piglet

        override this.ToString() =
            match this with
            | Cat -> "cat"
            | Dog -> "dog"
            | Piglet -> "piglet"

    type Pet = { species: Species; name: string }
    type Person = { firstName: string; lastName: string; pets: seq<Pet> }

    let PetPiglet (init: Pet) =
        Form.Return (fun s n -> { species = s; name = n })
        <*> Form.Yield init.species
        <*> (Form.Yield init.name
            |> Validation.IsNotEmpty "Please enter your pet's name.")

    let PersonPiglet (init: Person) =
        Form.Return (fun first last pets ->
            { firstName = first; lastName = last; pets = pets })
        <*> (Form.Yield init.firstName
            |> Validation.IsNotEmpty "Please enter your first name.")
        <*> (Form.Yield init.lastName
            |> Validation.IsNotEmpty "Please enter your last name.")
        <*> Form.Many init.pets { species = Cat; name = "" } PetPiglet
        |> Form.WithSubmit

    let RenderPet species name =
        Doc.Concat [
            div [attr.``class`` "field"] [
                label [attr.``class`` "label"] [
                    Doc.Radio [] Cat species; text (string Cat)
                ]
            ]
            div [attr.``class`` "field"] [
                label [attr.``class`` "label"] [
                    Doc.Radio [] Dog species; text (string Dog)
                ]
            ]
            div [attr.``class`` "field"] [
                label [attr.``class`` "label"] [
                    Doc.Radio [] Piglet species; text (string Piglet)
                ]
            ]
            div [attr.``class`` "field"] [
                Doc.Input [attr.``class`` "input"] name
            ]
        ]

    let ShowErrorsFor v =
        v
        |> View.Map (function
            | Success _ -> Doc.Empty
            | Failure errors ->
                Doc.Concat [
                    for error in errors do
                        yield b [attr.style "color:red"] [text error.Text]
                ]
        )
        |> Doc.EmbedView

    let RenderPerson (firstName: Var<string>)
                     (lastName: Var<string>)
                     (pets: Form.Many.CollectionWithDefault<Pet,_,_>)
                     (submit: Submitter<Result<_>>) =
        section [attr.``class`` "section"] [
            h2 [attr.``class`` "subtitle"] [text "You"]
            div [attr.``class`` "field"] [
                label [attr.``class`` "label"] [
                    text "First name: "
                    Doc.Input [attr.``class`` "input"] firstName
                ]
                ShowErrorsFor (submit.View.Through firstName)
            ]
            div [attr.``class`` "field"] [
                label [attr.``class`` "label"] [
                    text "Last name: "
                    Doc.Input [attr.``class`` "input"] lastName
                ]
                ShowErrorsFor (submit.View.Through lastName)
            ]
            h2 [attr.``class`` "subtitle"] [text "Your pets"]
            div [] [
                pets.Render (fun ops species name ->
                    div [attr.``class`` "field is-horizontal"] [
                        div [attr.``class`` "field-body"] [
                            RenderPet species name
                            Doc.ButtonValidate "Move up" [attr.``class`` "button"] ops.MoveUp
                            Doc.ButtonValidate "Move down" [attr.``class`` "button"] ops.MoveDown
                            Doc.Button "Delete" [attr.``class`` "button"] ops.Delete
                            ShowErrorsFor (submit.View.Through name)
                        ]
                    ])
                Doc.Button "Add a pet" [attr.``class`` "button"] pets.Add
            ]
            div [] [
                Doc.Button "Submit" [attr.``class`` "button"] submit.Trigger
            ]
        ]

    [<SPAEntryPoint>]
    let FinalForm() =
        PersonPiglet {
            firstName = ""
            lastName = ""
            pets = [||] }
        |> Form.Run (fun p ->
            let message =
                "Welcome to you " + p.firstName + " " + p.lastName +
                (p.pets
                    |> Seq.map (fun pet ->
                        ", your " + string pet.species + " " + pet.name)
                    |> String.concat "") +
                "!"
            JS.Alert message)
        |> Form.Render RenderPerson
        |> Doc.RunById "main"
