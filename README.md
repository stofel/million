million
=====

An OTP application

Build
-----
```
    Install erlang 23/24

    git clone https://github.com/erlang/rebar3.git
    cd rebar3
    ./bootstrap
    ./rebar3 local install

    cd some_path
    git clone https://github.com/stofel/million.git
    cd million
    $ make release
    $ make console
    And test!
    (million@localhost)1> million:test().
```
