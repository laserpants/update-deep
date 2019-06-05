'use strict';

require('./index.html');
require('./api.js');

var Elm = require('./src/Main.elm').Elm;

var storageKey = 'elm-update-deep-demo-app-session';

var session = sessionStorage.getItem(storageKey) || localStorage.getItem(storageKey);

var app = Elm.Main.init({
  node: document.getElementById('elm-code'),
  flags: {
    api: process.env.API_URL || 'http://localhost:4000',
    session: session || ''
  }
});

if (app.ports && app.ports.setSession) {
  app.ports.setSession.subscribe(function(data) {
    var api = data.user.rememberMe ? localStorage : sessionStorage;
    api.setItem(storageKey, JSON.stringify(data));
  });
}

if (app.ports && app.ports.clearSession) {
  app.ports.clearSession.subscribe(function(data) {
    localStorage.removeItem(storageKey);
    sessionStorage.removeItem(storageKey);
  });
}
