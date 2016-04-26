# Consolex

Consolex is a tool that allows you to attach a web based console to any mix project.

![Image of console](https://camo.githubusercontent.com/4fdc033fd49f42f5e1b2b43036c672d5a5a2f55c/687474703a2f2f692e696d6775722e636f6d2f4577706c59496e2e706e67)

Use the following mix task to start the server on port **5984**.

```
mix consolex.server
```

## Web Console Usage

Open up **localhost:5984** on your browser and by default, you get 2 launch options:

```
iex
iex -S mix
erl
```
You can choose to provide other shell options as well, like `iex -S mix phoenix.server`.
Once the shell is launched, you can start using the editor and hit "Ctrl/Cmd + Enter" or click on the provided button to send the code to the running IEx instance and execute it.

Consider a sample input code 

```
a = 1 + 2
b = a + 3
```

There are two ways to send the code to the shell for execution and a sample interaction with the shell is as follows

**Single line input(Default)**

```
iex(1)> a = 1 + 2; b = a + 3
6
```

**Multiline input**

```
iex(1)> a = 1 + 2
3
iex(2)> b = a + 3
6
```
Click on the options button to customise the console as per your liking.

__________________________________________________

**Warning: Due to the exposed shell for commands, it is strongly advised to refrain from using this in production environment**

**Warning: This project is still in development and contains bugs. Please help by reporting issues on github**
__________________________________________________

## Features

* **Web Console**
* Ability to plug into any existing mix project
* Multi line IEx command
* Command history
* Erl shell (Please set input option as "multi line")

## Installation

To attach the web console to any mix project, just add consolex to your list of dependencies.

Add consolex to your list of dependencies in `mix.exs` using either

`[{:consolex, "~> 0.0.3"}]`

`[{:consolex, git: "https://github.com/sivsushruth/consolex"}]`

## Copyright and License

Copyright (c) 2016, Sushruth Sivaramakrishnan.

Consolex source code is licensed under the MIT License.


