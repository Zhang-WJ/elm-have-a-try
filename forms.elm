import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)

main = 
    Html.beginnerProgram { model = model, view = view, update = update }


--MODEL

type alias Model =
   {name : String
   , password : String
   , passwordAgain : String}

model : Model
model = Model "" "" ""

--UPdate

type Msg = Name String | Password String | PasswordAgain String

update : Msg -> Model -> Model
update msg model =
    case msg of
        Name name ->
            {model | name = name }
        Password psd ->
            {model | password = psd }
        PasswordAgain psda ->
            {model | passwordAgain = psda }


--VIEW

view : Model -> Html Msg
view model = 
    div []
     [ input [ type_ "text", placeholder "Name", onInput Name ] []
     , input [ type_ "password", placeholder "Password", onInput Password ] []
     , input [ type_ "password", placeholder "Confirm password", onInput PasswordAgain ] []
     , viewValidation model ]


viewValidation : Model -> Html msg
viewValidation model =
    let 
        (color, message) = 
            if String.length model.password < 8 then
                ("red", "The password at leaat need 8!")
            --else if String.all Sting.isDigit model.password then
            --    ("red", "Password must have char!")
            else if  model.password == model.passwordAgain  then
                ("green", "OK")
            else
                ("red", "Passwords do not match!")
    in 
        div [ style [("color", color)] ] [ text message ] 
