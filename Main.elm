-- Read more about this program in the official Elm guide:
-- https://guide.elm-lang.org/architecture/user_input/buttons.html

import Html exposing (Html, program, div, button, text)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)
import List exposing (map)
import Time exposing (Time, millisecond)
import Math.Vector2 as V2 exposing(Vec2, vec2, getX, getY, scale, sub, add, dot, length, lengthSquared, normalize)
import Debug
import Keyboard

import AnimationFrame
import Collage exposing (Form)
import Color exposing (Color)
import Element exposing (toHtml)

type Msg = Update Time | KeyDown Int

type alias Model = { bodies: List RigidBody
                   , paused: Bool
                   }

--type alias Object a = { a | position: Vec2, velocity: Vec2, restitution: Float, invmass: Float }

--type alias Manifold a b = { a: Object a, b: Object b, penetration: Float, normal: Vec2 }
--type alias Rect = { min: Vec2, max: Vec2 }
--type alias Circle = { radius: Float }

type alias Manifold = (Float, Vec2)

type Shape = Circle Float
           | Rect Float Float

type alias AABB = { min: Vec2, max: Vec2 }

type alias RigidBody = { shape: Shape
                       , material: Material
                       , position: Vec2
                       , velocity: Vec2
                       --, acceleration: Vec2
                       --, angularVelocity: Float
                       --, orientation: Float
                       --, torque: Float
                       , color: Color
                       }

type Material = Rock
              | Wood
              | Metal
              | Bouncy
              | SuperBouncy
              | Pillow
              | Static
              | Custom { d: Float, r: Float }


bodyToAABB : RigidBody -> AABB
bodyToAABB { shape, position } =
  let x = getX position
      y = getY position
  in case shape of
    Circle r -> {
                  min = vec2 (x - r) (y - r)
                , max = vec2 (x + r) (y + r)
                }
    Rect width height -> {
                           min = vec2 (x - width / 2) (y - height / 2)
                         , max = vec2 (x + width / 2) (y + height / 2)
                         }

density : Material -> Float
density m = case m of
  Rock -> 0.6
  Wood -> 0.3
  Metal -> 1.2
  Bouncy -> 0.3
  SuperBouncy -> 0.3
  Pillow -> 0.1
  Static -> 0
  Custom { d } -> d


restitution : Material -> Float
restitution m = case m of
  Rock -> 0.1
  Wood -> 0.2
  Metal -> 0.05
  Bouncy -> 0.8
  SuperBouncy -> 0.95
  Pillow -> 0.2
  Static -> 0.4
  Custom { r } -> r

volume : Shape -> Float
volume s = case s of
  Circle r ->
    r^2 * pi
  Rect width height ->
    width * height

mass : RigidBody -> Float
mass body = volume body.shape * density body.material

invmass : RigidBody -> Float
invmass body =
  let m = mass body
  in if m == 0 then 0 else 1 / mass body

gravity = vec2 0 -9.82

main =
  program {
            init = init
          , view = view
          , update = update
          , subscriptions = subscriptions
          }

initBodies = [
  {
    shape = Circle 20
  , position = vec2 50 50
  , velocity = vec2 0 0
  , material = Bouncy
  , color = Color.blue
  },
  {
    shape = Rect 50 30
  , position = vec2 -50 50
  , velocity = vec2 0 0
  , material = SuperBouncy
  , color = Color.red
  },
  {
    shape = Rect 25 25
  , position = vec2 -50 100
  , velocity = vec2 0 0
  , material = Bouncy
  , color = Color.yellow
  },
  {
    shape = Rect 250 50
  , position = vec2 0 -100
  , velocity = vec2 0 0
  , material = Static
  , color = Color.green
  }
  ]


init : (Model, Cmd Msg)
init = ({ bodies = initBodies, paused = False }, Cmd.none)

bodyToForm : RigidBody -> Form
bodyToForm { shape, position, color } = case shape of
  Circle r ->
    Collage.circle r
      |> Collage.filled color
      |> Collage.move (getX position, getY position)
  Rect width height ->
    Collage.rect width height
      |> Collage.filled color
      |> Collage.move(getX position, getY position)


view : Model -> Html Msg
view { bodies } =
  div [ style [("border", "1px solid #ddd"), ("display", "inline-block")] ]
    [
      Collage.collage 500 500 (map bodyToForm bodies) |> toHtml
    --, button [ onClick AddCircle ] [ text "Add Circle" ]
    ]


subscriptions : Model -> Sub Msg
subscriptions model = Sub.batch [ Keyboard.downs KeyDown, AnimationFrame.diffs Update ]


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Update t -> (if model.paused then model else updatePhysics (Time.inSeconds t) model, Cmd.none)
    KeyDown 80 -> Debug.log "paused" ({ model | paused = not model.paused }, Cmd.none)
    KeyDown _ -> (model, Cmd.none)



updatePhysics : Float -> Model -> Model
updatePhysics dt model = { model | bodies = (handleCollisions (map (updateBodyEuler dt) model.bodies)) }

force body = vec2 0 0

getAcceleration : RigidBody -> Vec2
getAcceleration body =
  let im = invmass body
  in
      if invmass body == 0 then
        vec2 0 0
      else
        add gravity (scale im (force body))

updateBodyEuler : Float -> RigidBody -> RigidBody
updateBodyEuler dt body =
  let velocity = scale dt (getAcceleration body) |> add body.velocity
      position = scale dt velocity |> add body.position
  in { body | velocity = velocity, position = position }

updateBody : Float -> RigidBody -> RigidBody
updateBody dt body =
  let acceleration = getAcceleration body
      position = add body.position (scale dt (add velocity (scale (dt / 2) acceleration)))
      velocity = add body.velocity (scale dt acceleration)
      newAcceleration = getAcceleration body
  in { body | velocity = add velocity (scale (dt / 2) (sub newAcceleration acceleration)), position = position }

--satRect : Rect -> Rect -> Bool
--satRect a b =
--  if (getX a.max) < (getX b.min) || (getX a.min) > (getX b.max) then
--    False
--  else if (getY a.max) < (getY b.min) || (getY a.min) > (getY b.max) then
--    False
--  else
--    True

--circleCircleCollision : Circle -> Circle -> Bool
--circleCircleCollision a b =
--  let r = (a.radius + b.radius)^2
--      ax = getX a.position
--      ay = getY a.position
--      bx = getX b.position
--      by = getY b.position
--  in r < (ax + bx)^2 + (ay + by)^2

type alias CircleSpec = (Float, Vec2)
type alias RectSpec = (AABB, Vec2)

handleCollisions : List RigidBody -> List RigidBody
handleCollisions bodies = case bodies of
  [] -> []
  (body::rest) ->
    let (body_resolved, rest_resolved) = handleSingle body rest
    in body_resolved::(handleCollisions rest_resolved)

handleSingle : RigidBody -> List RigidBody -> (RigidBody, List RigidBody)
handleSingle body bodies = case bodies of
  [] -> (body, [])
  (other::rest) -> case collision body other of
    Just manifold ->
      let (body_resolved, other_resolved) = resolveCollision body other manifold
          (body_resolved_all, rest_resolved) = handleSingle body_resolved rest
      in (body_resolved_all, other_resolved::rest_resolved)
    Nothing ->
      let (body_resolved, rest_resolved) = handleSingle body rest
      in (body_resolved, other::rest_resolved)

--handleSingleRec : RigidBody -> List RigidBody -> List RigidBody
--handleSingleRec a bodies = case bodies of
--  [] -> []
--  (b:bs) -> case collision a b of
--    Just m ->
--      let (a', b') = resolveCollision a b m
--      in b'::(handleSingle a' bs)
--    Nothing -> b::(handleSingle a bs)

--bodyCollisions : RigidBody -> List RigidBody -> (List (RigidBody, Manifold), List RigidBody)
--bodyCollisions a bodies = case bodies of
--  [] -> ([], [])
--  (b::bs) -> case collision a b of
--    Just m -> (a, b, m)::(bodyCollisions a bs)
--    Nothing -> bodyCollisions a bs

collision : RigidBody -> RigidBody -> Maybe Manifold
collision a b = case a.shape of
  Circle r -> case b.shape of
    Circle r2 -> circleCircleCollision (r, a.position) (r2, b.position)
    Rect _ _ -> circleRectCollision (r, a.position) (bodyToAABB b, b.position)
  Rect _ _ -> case b.shape of
    Circle r -> rectCircleCollision (bodyToAABB a, a.position) (r, b.position)
    Rect _ _ -> rectRectCollision (bodyToAABB a, a.position) (bodyToAABB b, b.position)

extent g o = (g o.max - g o.min) / 2

rectRectCollision : RectSpec -> RectSpec -> Maybe Manifold
rectRectCollision (a, apos) (b, bpos) =
  let n = sub bpos apos
      x_overlap = extent getX a + extent getX b - abs (getX n)
      y_overlap = extent getY a + extent getY b - abs (getY n)
  in
      if x_overlap > 0 && y_overlap > 0 then
        if x_overlap < y_overlap then
          Just (x_overlap, vec2 (if getX n < 0 then -1 else 1) 0)
        else
          Just (y_overlap, vec2 0 (if getY n < 0 then -1 else 1))
      else
        Nothing

resolveClosest : AABB -> Vec2 -> (Bool, Vec2)
resolveClosest box normal =
  let x_extent = extent getX box
      y_extent = extent getY box
      closest = vec2 (clamp -x_extent x_extent (getX normal)) (clamp -y_extent y_extent (getY normal))
  in
      if closest /= normal then
        (False, closest)
      else
        if abs (getX normal) < abs (getY normal) then
          (True, vec2 (if getX closest > 0 then x_extent else -x_extent) (getY closest))
        else
          (True, vec2 (getX closest) (if getY closest > 0 then y_extent else -y_extent))

circleRectCollision : CircleSpec -> RectSpec -> Maybe Manifold
circleRectCollision c r = case rectCircleCollision r c of
  Nothing -> Nothing
  Just (penetration, normal) -> Just (penetration, V2.negate normal)

rectCircleCollision : RectSpec -> CircleSpec -> Maybe Manifold
rectCircleCollision (box, apos) (r, bpos) =
  let n = sub bpos apos
      (is_inside, closest) = resolveClosest box n
      normal = sub n closest
      d = lengthSquared normal
  in
      if d > r^2 && not is_inside then
        Nothing
      else
        let l = sqrt d
            n2 = normalize normal |> Debug.log "normal"
        in Just (r - l, if is_inside then V2.negate n2 else n2) |> Debug.log "result"


circleCircleCollision : CircleSpec -> CircleSpec -> Maybe Manifold
circleCircleCollision (ar, apos) (br, bpos) =
  let n = sub bpos apos
      r = (ar + br)^2
  in
      if lengthSquared n > r then
        Nothing
      else
        let d = length n in
          if d == 0 then
            Just (ar, vec2 1 0)
          else
            Just (r - d, scale (1 / d) n)


correctPosition : RigidBody -> RigidBody -> Manifold -> (RigidBody, RigidBody)
correctPosition a b (penetration, normal) =
  let percent = 0.8
      slop = 0.02
      c = ((max (penetration - slop) 0) / (invmass a + invmass b)) * percent
      correction = scale c normal
  in
    ( { a | position = sub a.position (scale (invmass a) correction) }
    , { b | position = add b.position (scale (invmass b) correction) }
    )


resolveCollision : RigidBody -> RigidBody -> Manifold -> (RigidBody, RigidBody)
resolveCollision a b m =
  let normal = Tuple.second m |> Debug.log "normal"
      relativeVelocity = sub b.velocity a.velocity
      normalVelocity = dot relativeVelocity normal
      e = min (restitution a.material) (restitution b.material)
      j = -(1 + e) * normalVelocity / (invmass a + invmass b)
      impulse = scale j normal |> Debug.log "impulse"
  in
      if normalVelocity > 0 then
        correctPosition a b m
      else
        correctPosition
          { a | velocity = sub a.velocity (scale (invmass a) impulse) |> Debug.log "anew" }
          { b | velocity = add b.velocity (scale (invmass b) impulse) |> Debug.log "bnew" }
          m

