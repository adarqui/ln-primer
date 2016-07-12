# ln-primer

A starting point for my LN project.

## what is LN?

LN is simply the name of the "software system" i'm using to re-build my website (adarq.org). LN will
provide various services to adarq.org members: Forum, Training History, Training Data Analysis,
Competitions, Badges, Groups, Organizations, Workout Templates, Learning Tools, Blogs, CLI tools,
API, etc.

LN is my circus.

## adarq.org history

Historically (and currently), adarq.org is a forum dedicated towards improving athleticism. It
was launched atop well known forum software known as SMF (Simple Machines Forum) which is written
in PHP.

## LN stack

Right now, LN is built entirely in haskell and purescript.

## infrastructure

Linux -> HAProxy -> LXC

## backend

Haskell -> Yesod -> Persistent

Postgres, Redis, RabbitMQ

## frontend

Purescript -> Halogen -> Halogen Bootstrap3

## repositories

All of the reposotiroes below are involved in this project.  Eventually I might move all of this to the "adarq" organization.

## public repositories

Haskell:
- https://github.com/adarqui/ln-types
- https://github.com/adarqui/haskell-ln-types (generated, replaces ln-types)
- https://github.com/adarqui/ln-lib
- https://github.com/adarqui/ln-interop
- https://github.com/adarqui/ln-validate
- https://github.com/adarqui/ln-sanitize
- https://github.com/adarqui/ln-yesod
- https://github.com/adarqui/ln-api
- https://github.com/adarqui/ln-api-runner
- https://github.com/adarqui/ln-smf-migration

- https://github.com/adarqui/haskell-interop-prime
- https://github.com/adarqui/haskell-api-helpers
- https://github.com/adarqui/haskell-ebyam
- https://github.com/adarqui/haskell-rehtie
- https://github.com/adarqui/haskell-ifte


## private repositories

These repos are private until I get this project launched.

Haskell:
- https://github.com/adarqui/ln-fix

## deprecated repositories

My project has grown too large for the purescript compiler. Unfortunately, compile times are too slow. Thus, i'm now migrating everything to 100% haskell.

Purescript:
- https://github.com/adarqui/purescript-ln (deprecated)
- https://github.com/adarqui/purescript-ln-types
- https://github.com/adarqui/purescript-api
- https://github.com/adarqui/purescript-api-helpers
- https://github.com/adarqui/purescript-date-helpers
- https://github.com/adarqui/purescript-bbcode-parser
- https://github.com/adarqui/purescript-ebyam
- https://github.com/adarqui/purescript-rehtie

## code generation

A rather large amount of code is generated via haskell-interop-prime and ln-interop. This is because I need to communicate seamlessly
between haskell and purescript. Here are the automatically generated source files:

Haskell:
- https://github.com/adarqui/ln-types/blob/master/src/LN/T/Internal/JSON.hs

Purescript:
- https://github.com/adarqui/purescript-ln/blob/master/src/LN/T/Internal/Types.purs
- https://github.com/adarqui/purescript-ln/blob/master/src/LN/T/Internal/Convert.purs
- https://github.com/adarqui/purescript-ln/blob/master/src/LN/Api/Internal.purs
- https://github.com/adarqui/purescript-ln/blob/master/src/LN/Api/Internal/String.purs

There's quite a bit of code generated, and it will only continue to grow. I'm not necessarily "proud" of this, but, I don't really
see any other way to achieve this small level of haskell <-> purescript interop. I have recently made slight optimizations by
removing unnecessary instances on both the Purescript & Haskell sides. However, it barely makes a dent.

Finally, I do think the code-gen tools will pay off. Eventually I may want to produce equivalent records & api routines for
other languages (ocaml, go, js).

## outro

pc.

-- adarqui
