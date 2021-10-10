## Embassy

### API

`embassy.vdom.core/apply-vdom` is the main function of the library, it applies a `vdom-diff`
data structure  (simply called `vdom` throughout the source code) to a `dom-element`.

A `vdom-diff` is created using the helper functions in the namespace `embassy.vdom.helper`.
See the namespace [embassy.vdom.helper-test](test/embassy/vdom/helper_test.cljc) for examples on how to use them.

`embassy.vdom.core/comp` is the second most important function of the library, it returns
a composition of 2 `vdom-diff` data structures.

### Use cases

This library can be used for updating the dom in a browser from a remote location,
in an incremental way. For example, by sending the updates from a server connected
to the browser via a websocket.

## The demo application

Compile the source code via Shadow-CLJS:

```shell
npm install
npx shadow-cljs watch :app
```

Then browse the page at [http://localhost:3000/](http://localhost:3000/)

## Running the tests while developing

```shell
./bin/kaocha --watch
```
