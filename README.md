Prolog for JCU
==============

This package was developed to demonstrate the ideas behind the Prolog language.
It uses a very small interpreter (*Language.Prolog.Nanoprolog*) which can be
run on its own.

This package contains an environment constructed for the Junior College at
Utrecht University. It provides a simple environment in which rules can be
defined, and proofs can be constructed interactively. The software can be
installed on a server, so students do not have to install anything on their own
machines.


Installation instructions
=========================

This software has been tested with Haskell Platform 2011.2.0.1 on Mac
OS X 10.7.2 and GHC 7.2.1 (64-bit).

To install the JCU package in development mode, issue a *make* command in the
project root. To install it in production mode, just issue cabal install in the
project root.


Usage instructions
==================
Before you can use the application, you have to start it and register an
account.

Starting
--------
After installation, just issue the

> jcu

command. If you want to run the application in development mode, you should
issue this command in the root directory of the project.

Accessing
---------
The application can be viewed from your browser on the following address:

http://localhost:8000/

Registering
-----------
Before you can use the application, you have to register an account. This
can be done at

http://localhost:8000/signup

Your username must be a valid email address and your password needs to be at
least six characters long.

Logging in
----------
After signup, you can log in at

http://localhost:8000/login

using the credentials you have just entered during registration.

Using
-----
After logging in, the main screen is visible. It is divided in two sections.
On the left-hand side we have the proof tree and on the right-hand side
the list of rules.
(TODO: Finish this bit)
