# Consolex

Consolex is a tool that allows you to attach a web based console to any mix project.

![Image of console](https://camo.githubusercontent.com/868e1d520c1e46898d34270cbb773d20483177de/687474703a2f2f692e696d6775722e636f6d2f795369647970472e706e67)

Use the following mix task to start the server on port **5984**.

```
mix consolex.server
```

Open up **localhost:5984** on your browser and by default, you get 2 launch options: 

```
iex
iex -S mix
```
You can choose to provide other shell options as well, like `iex -S mix phoenix.server`.
Once the shell is launched, you can start using the editor and hit "Ctrl/Cmd + Enter" or click on the provided button to send the code to the running IEx instance and execute it. 

**Warning: This project is still in development and contains bugs. Please help by reporting issues on github**

## Features

* **Web Console**
* Ability to plug into any existing mix project
* Multi line IEx command
* Command history


## Installation

To attach the web console to any mix project, just add consolex to your list of dependencies.

Add consolex to your list of dependencies in `mix.exs` using either 

`[{:consolex, "~> 0.0.2"}]` 

`[{:consolex, git: "https://github.com/sivsushruth/consolex"}]`
    
## Copyright and License

Copyright (c) 2014, Sushruth Sivaramakrishnan.

Consolex source code is licensed under the MIT License.    


