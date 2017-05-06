-- Read more about this program in the official Elm guide:
-- https://guide.elm-lang.org/architecture/user_input/buttons.html

import Html exposing (Html, program, div, button, text)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)
import List exposing (map)
import Time exposing (Time, millisecond)
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr
import Math.Vector2 as V2 exposing(Vec2, vec2, getX, getY, scale, sub, add, dot, length, lengthSquared)

type Msg = Update Time

type alias Model = { entities: List (Object Circle)
                   , lastFrame: Time
                   , accumulator: Float
                   }

type alias Object a = { a | position: Vec2, velocity: Vec2, restitution: Float, invmass: Float }

type alias Manifold a b = { a: Object a, b: Object b, penetration: Float, normal: Vec2 }
type alias Rect = { min: Vec2, max: Vec2 }
type alias Circle = { radius: Float }

--type Shape = Circle { r: Float }
--| Rect { min: Vec2, max: Vec2 }

main =
  program {
            init = init
          , view = view
          , update = update
          , subscriptions = subscriptions
          }

initEntities = [{ position = vec2 50 50, radius = 10, velocity = vec2 0 0, invmass = 0, restitution = 0.1 }]


fps = 100
dt = 1 / fps
accThreshold = 0.2

init : (Model, Cmd Msg)
init = ({ entities = initEntities, lastFrame = 0, accumulator = 0 }, Cmd.none)


toSvg : Object Circle -> Svg Msg
toSvg e = case e of
  { position, radius } ->
    let x = toString (getX position)
        y = toString (getY position)
        r = toString radius
    in Svg.circle [SvgAttr.cx x, SvgAttr.cy y, SvgAttr.r r, SvgAttr.fill "#0B79CE"] []


toSvgs : List (Object Circle) -> List (Svg Msg)
toSvgs es = map toSvg es


svgAttr : List (Svg.Attribute Msg)
svgAttr = [ SvgAttr.viewBox "0 0 100 100"
          , SvgAttr.width "500px"
          , SvgAttr.height "500px"
          , style [("border", "1px solid black")]
          ]


genSvg : List (Object Circle) -> Html Msg
genSvg es = Svg.svg svgAttr (map toSvg es)


view : Model -> Html Msg
view { entities } =
  div []
    [
      genSvg entities
    --, button [ onClick AddCircle ] [ text "Add Circle" ]
    ]


subscriptions : Model -> Sub Msg
subscriptions model = Time.every millisecond Update


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Update t -> if model.lastFrame == 0 then
      ({ model | lastFrame = t }, Cmd.none)
    else
      (updateGame t model, Cmd.none)


updateGame : Time -> Model -> Model
updateGame t model = stepPhysics (model.accumulator + (Time.inSeconds t - Time.inSeconds model.lastFrame)) model


stepPhysics : Float -> Model -> Model
stepPhysics acc model =
  if acc > accThreshold then
    stepPhysics accThreshold (updatePhysics dt model)
  else if acc > dt then
    stepPhysics (acc - dt) (updatePhysics dt model)
  else
    model

updatePhysics : Float -> Model -> Model
updatePhysics dt model = model

satRect : Rect -> Rect -> Bool
satRect a b =
  if (getX a.max) < (getX b.min) || (getX a.min) > (getX b.max) then
    False
  else if (getY a.max) < (getY b.min) || (getY a.min) > (getY b.max) then
    False
  else
    True

extent g o = (g o.max - g o.min) / 2

--circleCircleCollision : Circle -> Circle -> Bool
--circleCircleCollision a b =
--  let r = (a.radius + b.radius)^2
--      ax = getX a.position
--      ay = getY a.position
--      bx = getX b.position
--      by = getY b.position
--  in r < (ax + bx)^2 + (ay + by)^2

rectRectCollision : Object Rect -> Object Rect -> Maybe (Manifold Rect Rect)
rectRectCollision a b =
  let n = sub b.position a.position
      x_overlap = extent getX a + extent getX b - abs (getX n)
      y_overlap = extent getY a + extent getY b - abs (getY n)
  in
      if x_overlap > 0 && y_overlap > 0 then
        if x_overlap > y_overlap then
          Just { a = a, b = b, penetration = x_overlap, normal = vec2 (if getX n < 0 then -1 else 1) 0 }
        else
          Just { a = b, b = b, penetration = y_overlap, normal = vec2 0 (if getY n < 0 then -1 else 1) }
      else
        Nothing

resolveClosest : Object Rect -> Object Circle -> Vec2 -> (Bool, Vec2)
resolveClosest a b n =
  let x_extent = extent getX a
      y_extent = extent getY a
      closest = vec2 (clamp -x_extent x_extent (getX n)) (clamp -y_extent y_extent (getY n))
  in
      if closest /= n then
        (False, closest)
      else
        if abs (getX n) > abs (getY n) then
          (True, vec2 (if getX closest > 0 then x_extent else -x_extent) (getY closest))
        else
          (True, vec2 (getX closest) (if getY closest > 0 then y_extent else -y_extent))


rectCircleCollision : Object Rect -> Object Circle -> Maybe (Manifold Rect Circle)
rectCircleCollision a b =
  let n = sub b.position a.position
      (is_inside, closest) = resolveClosest a b n
      normal = sub n closest
      r = b.radius
  in
      if (lengthSquared normal) > r^2 && not is_inside then
        Nothing
      else
        let d = length normal
        in Just { a = a, b = b, penetration = r - d, normal = if is_inside then V2.negate n else n }


circleCircleCollision : Object Circle -> Object Circle -> Maybe (Manifold Circle Circle)
circleCircleCollision a b =
  let n = sub b.position a.position
      r = (a.radius + b.radius)^2
  in if lengthSquared n > r then Nothing else
    let d = length n
    in if d == 0 then Just { a = a, b = b, penetration = a.radius, normal = vec2 1 0 }
       else Just { a = a, b = b, penetration = r - d, normal = scale (1 / d) n }


correctPosition : Object a -> Object b -> Float -> Vec2 -> (Object a, Object b)
correctPosition a b penetration normal =
  let percent = 0.2
      slop = 0.01
      c = max (penetration - slop) 0 / (a.invmass + b.invmass) * percent
      correction = scale c normal
  in
    ( { a | position = sub a.position (scale a.invmass correction) }
    , { b | position = add b.position (scale b.invmass correction) }
    )


resolveCollision : Manifold a b -> (Object a, Object b)
resolveCollision { a, b, penetration, normal } =
  let relativeVelocity = sub b.velocity a.velocity
      normalVelocity = dot relativeVelocity normal
      e = min a.restitution b.restitution
      j = -(1 + e) * normalVelocity / (a.invmass + b.invmass)
      impulse = scale j normal
  in
      if normalVelocity > 0 then
        (a, b)
      else
        ( { a | velocity = sub a.velocity (scale a.invmass impulse) }
        , { b | velocity = sub b.velocity (scale a.invmass impulse) }
        )

