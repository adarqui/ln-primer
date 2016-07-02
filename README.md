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
- https://github.com/adarqui/ln-lib
- https://github.com/adarqui/ln-interop
- https://github.com/adarqui/ln-validate
- https://github.com/adarqui/ln-sanitize
- https://github.com/adarqui/ln-api
- https://github.com/adarqui/ln-api-runner
- https://github.com/adarqui/ln-smf-migration

- https://github.com/adarqui/haskell-interop-prime
- https://github.com/adarqui/haskell-api-helpers
- https://github.com/adarqui/haskell-ebyam
- https://github.com/adarqui/haskell-rehtie
- https://github.com/adarqui/haskell-ifte

Purescript:
- https://github.com/adarqui/purescript-ln
- https://github.com/adarqui/purescript-api-helpers
- https://github.com/adarqui/purescript-date-helpers
- https://github.com/adarqui/purescript-bbcode-parser
- https://github.com/adarqui/purescript-ebyam
- https://github.com/adarqui/purescript-rehtie

## private repositories

These repos are private until I get this project launched.

Haskell:
- https://github.com/adarqui/ln-fix
- https://github.com/adarqui/ln-yesod

Purescript:
- https://github.com/adarqui/ln-ui
