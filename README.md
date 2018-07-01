# purescript-comonad-rss

RSS feed reader written for the second part of my bachelor thesis: [A Real-World Application with a Comonadic User Interface](https://arthurxavierx.github.io/RealWorldAppComonadicUI.pdf), which features an extension of the model presented by Phil Freeman on [Declarative UIs are the Future — And the Future is Comonadic!](http://functorial.com/the-future-is-comonadic/main.pdf) with support for asynchronous effects in response to user interaction and composition of components with powerful communication schemes.

![Screenshot](https://raw.githubusercontent.com/arthurxavierx/purescript-comonad-rss/master/corss.png)

## Build instructions

In order to build this application one must have the [PureScript](http://www.purescript.org/) compiler (version 0.12.0) installed, as well as the [npm](https://www.npmjs.com/) and [bower](https://bower.io/) tools for package management.

After installing the prerequisites, run:

```
make
```

## TODOs

- [x] Load feeds from local storage;
- [ ] Save new feeds to local storage;
- [ ] Add link to original article in RSS items;
