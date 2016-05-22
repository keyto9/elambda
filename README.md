# elambda evaluator


## Installation && Run

- install erlang
    ```bash
    $ yum -y install erlang
    ```

- build elambda
    ```bash
    $ git clone https://github.com/keyto9/elambda.git
    $ cd elambda/ && ./rebar get-deps
    $ cd ./deps/mochiweb/ && ./rebar compile
    $ cd ../../ && ./rebar compile

    ```
note: you can config in `elambda_cfg.erl` before building.

- run start script
    ```bash
    $ sh start.sh
    ```


## How to Query

- evaluate
    ```bash
    $ curl "http://127.0.0.1:10086/evaluate?lambda=((add 2) 3)"
    ```

- verify
    ```bash
    $ curl "http://127.0.0.1:10086/verify?lambda=\x.((add 1) x)"
    ```

