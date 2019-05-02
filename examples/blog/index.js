'use strict';

require('./index.html');

var Elm = require('./src/Main.elm').Elm;

var app = Elm.Main.init({
  node: document.getElementById('elm-code'),
  flags: {
    api: process.env.API_URL || 'http://localhost:4000'
  }
});
