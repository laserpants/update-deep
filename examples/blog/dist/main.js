/******/ (function(modules) { // webpackBootstrap
/******/ 	// The module cache
/******/ 	var installedModules = {};
/******/
/******/ 	// The require function
/******/ 	function __webpack_require__(moduleId) {
/******/
/******/ 		// Check if module is in cache
/******/ 		if(installedModules[moduleId]) {
/******/ 			return installedModules[moduleId].exports;
/******/ 		}
/******/ 		// Create a new module (and put it into the cache)
/******/ 		var module = installedModules[moduleId] = {
/******/ 			i: moduleId,
/******/ 			l: false,
/******/ 			exports: {}
/******/ 		};
/******/
/******/ 		// Execute the module function
/******/ 		modules[moduleId].call(module.exports, module, module.exports, __webpack_require__);
/******/
/******/ 		// Flag the module as loaded
/******/ 		module.l = true;
/******/
/******/ 		// Return the exports of the module
/******/ 		return module.exports;
/******/ 	}
/******/
/******/
/******/ 	// expose the modules object (__webpack_modules__)
/******/ 	__webpack_require__.m = modules;
/******/
/******/ 	// expose the module cache
/******/ 	__webpack_require__.c = installedModules;
/******/
/******/ 	// define getter function for harmony exports
/******/ 	__webpack_require__.d = function(exports, name, getter) {
/******/ 		if(!__webpack_require__.o(exports, name)) {
/******/ 			Object.defineProperty(exports, name, { enumerable: true, get: getter });
/******/ 		}
/******/ 	};
/******/
/******/ 	// define __esModule on exports
/******/ 	__webpack_require__.r = function(exports) {
/******/ 		if(typeof Symbol !== 'undefined' && Symbol.toStringTag) {
/******/ 			Object.defineProperty(exports, Symbol.toStringTag, { value: 'Module' });
/******/ 		}
/******/ 		Object.defineProperty(exports, '__esModule', { value: true });
/******/ 	};
/******/
/******/ 	// create a fake namespace object
/******/ 	// mode & 1: value is a module id, require it
/******/ 	// mode & 2: merge all properties of value into the ns
/******/ 	// mode & 4: return value when already ns object
/******/ 	// mode & 8|1: behave like require
/******/ 	__webpack_require__.t = function(value, mode) {
/******/ 		if(mode & 1) value = __webpack_require__(value);
/******/ 		if(mode & 8) return value;
/******/ 		if((mode & 4) && typeof value === 'object' && value && value.__esModule) return value;
/******/ 		var ns = Object.create(null);
/******/ 		__webpack_require__.r(ns);
/******/ 		Object.defineProperty(ns, 'default', { enumerable: true, value: value });
/******/ 		if(mode & 2 && typeof value != 'string') for(var key in value) __webpack_require__.d(ns, key, function(key) { return value[key]; }.bind(null, key));
/******/ 		return ns;
/******/ 	};
/******/
/******/ 	// getDefaultExport function for compatibility with non-harmony modules
/******/ 	__webpack_require__.n = function(module) {
/******/ 		var getter = module && module.__esModule ?
/******/ 			function getDefault() { return module['default']; } :
/******/ 			function getModuleExports() { return module; };
/******/ 		__webpack_require__.d(getter, 'a', getter);
/******/ 		return getter;
/******/ 	};
/******/
/******/ 	// Object.prototype.hasOwnProperty.call
/******/ 	__webpack_require__.o = function(object, property) { return Object.prototype.hasOwnProperty.call(object, property); };
/******/
/******/ 	// __webpack_public_path__
/******/ 	__webpack_require__.p = "";
/******/
/******/
/******/ 	// Load entry module and return exports
/******/ 	return __webpack_require__(__webpack_require__.s = "./index.js");
/******/ })
/************************************************************************/
/******/ ({

/***/ "./index.html":
/*!********************!*\
  !*** ./index.html ***!
  \********************/
/*! no static exports found */
/***/ (function(module, exports, __webpack_require__) {

eval("module.exports = __webpack_require__.p + \"index.html\";\n\n//# sourceURL=webpack:///./index.html?");

/***/ }),

/***/ "./index.js":
/*!******************!*\
  !*** ./index.js ***!
  \******************/
/*! no static exports found */
/***/ (function(module, exports, __webpack_require__) {

"use strict";
eval("/* WEBPACK VAR INJECTION */(function(process) {\n\n__webpack_require__(/*! ./index.html */ \"./index.html\");\n\nvar Elm = __webpack_require__(/*! ./src/Main.elm */ \"./src/Main.elm\").Elm;\n\nvar app = Elm.Main.init({\n  node: document.getElementById('elm-code'),\n  flags: {\n    api: process.env.API_URL || 'http://localhost:4000'\n  }\n});\n\n// //var ws = new WebSocket(\"wss://echo.websocket.org\");\n// //\n// //ws.onmessage = function(message) {\n// //  console.log(message);\n// //  app.ports.websocketIn.send(JSON.stringify({\n// //    data: message.data,\n// //    timeStamp: message.timeStamp\n// //  }));\n// //};\n// \n// var usernames = [\n//   'test',\n//   'diego',\n//   'neo',\n//   'deepak',\n//   'moses',\n//   'kevin',\n//   'knuth',\n//   'laserpants',\n//   'angelo'\n// ];\n// \n// if (app.ports && app.ports.websocketOut && app.ports.websocketIn) {\n// \n//   app.ports.websocketOut.subscribe(function(message) {\n//     var data = JSON.parse(message);\n//     if ('is_login_available' === data.type) {\n//       var login = data.payload;\n//       setTimeout(function() {\n//         app.ports.websocketIn.send(JSON.stringify({\n//           type: -1 === usernames.indexOf(login) ? 'login_available' : 'login_taken',\n//           payload: { login: login }\n//         }));\n//       }, 250);\n//     }\n// \n//   });\n// \n// }\n// \n// // Fake API ///////////////////////////////////////////////////////////////////\n// \n// var users =\n// [\n//   {\n//     id: 1,\n//     name: '',\n//     password: 'test',\n//     login: 'test'\n//   }\n// ];\n// \n// var usersId = 2;\n// \n// var posts =\n// [\n//   {\n//     id: 1,\n//     title: '1',\n//     body: 'hello',\n//     comments: []\n//   }\n// ];\n// \n// var postsId = 2;\n// \n// var delay = 800;\n// \n// xhook.before(function(request, callback) {\n// \n//   if (request.url.endsWith('posts') && 'GET' === request.method) {\n// \n//     // GET /posts\n// \n//     setTimeout(function() {\n//       callback({\n//         status: 200,\n//         data: JSON.stringify({ posts: posts }),\n//         headers: { 'Content-Type': 'application/json' }\n//       });\n//     }, delay);\n// \n//   } else if (request.url.endsWith('posts') && 'POST' === request.method) {\n// \n//     // POST /posts\n// \n//     setTimeout(function() {\n//       var post = JSON.parse(request.body);\n//       post.id = postsId++;\n//       post.comments = [];\n//       posts.push(post);\n//       callback({\n//         status: 200,\n//         data: JSON.stringify({ post: post }),\n//         headers: { 'Content-Type': 'application/json' }\n//       });\n//     }, delay);\n// \n//   } else if (request.url.endsWith('auth/login') && 'POST' === request.method) {\n// \n//     // POST /auth/login\n// \n//     setTimeout(function() {\n//       var params = JSON.parse(request.body);\n//       var filtered = users.filter(function(user) {\n//         return user.login === params.login && user.password === params.password;\n//       });\n//       if (filtered.length > 0) {\n//         var responseData = filtered[0];\n//         responseData.token = 'fake-jwt-token';\n//         console.log(responseData);\n//         callback({\n//           status: 200,\n//           data: JSON.stringify({ user: responseData }),\n//           headers: { 'Content-Type': 'application/json' }\n//         });\n//       } else {\n//         console.log('401 Unauthorized');\n//         callback({\n//           status: 401,\n//           data: JSON.stringify({ error: 'Unauthorized' }),\n//           headers: { 'Content-Type': 'application/json' }\n//         });\n//       }\n//     }, delay);\n// \n//   } else if (request.url.endsWith('auth/register') && 'POST' === request.method) {\n// \n//     // POST /auth/register\n// \n//     setTimeout(function() {\n// \n//     }, delay);\n// \n//   }else {\n//     callback();\n//   }\n// \n// });\n\n/* WEBPACK VAR INJECTION */}.call(this, __webpack_require__(/*! ./node_modules/process/browser.js */ \"./node_modules/process/browser.js\")))\n\n//# sourceURL=webpack:///./index.js?");

/***/ }),

/***/ "./node_modules/process/browser.js":
/*!*****************************************!*\
  !*** ./node_modules/process/browser.js ***!
  \*****************************************/
/*! no static exports found */
/***/ (function(module, exports) {

eval("// shim for using process in browser\nvar process = module.exports = {};\n\n// cached from whatever global is present so that test runners that stub it\n// don't break things.  But we need to wrap it in a try catch in case it is\n// wrapped in strict mode code which doesn't define any globals.  It's inside a\n// function because try/catches deoptimize in certain engines.\n\nvar cachedSetTimeout;\nvar cachedClearTimeout;\n\nfunction defaultSetTimout() {\n    throw new Error('setTimeout has not been defined');\n}\nfunction defaultClearTimeout () {\n    throw new Error('clearTimeout has not been defined');\n}\n(function () {\n    try {\n        if (typeof setTimeout === 'function') {\n            cachedSetTimeout = setTimeout;\n        } else {\n            cachedSetTimeout = defaultSetTimout;\n        }\n    } catch (e) {\n        cachedSetTimeout = defaultSetTimout;\n    }\n    try {\n        if (typeof clearTimeout === 'function') {\n            cachedClearTimeout = clearTimeout;\n        } else {\n            cachedClearTimeout = defaultClearTimeout;\n        }\n    } catch (e) {\n        cachedClearTimeout = defaultClearTimeout;\n    }\n} ())\nfunction runTimeout(fun) {\n    if (cachedSetTimeout === setTimeout) {\n        //normal enviroments in sane situations\n        return setTimeout(fun, 0);\n    }\n    // if setTimeout wasn't available but was latter defined\n    if ((cachedSetTimeout === defaultSetTimout || !cachedSetTimeout) && setTimeout) {\n        cachedSetTimeout = setTimeout;\n        return setTimeout(fun, 0);\n    }\n    try {\n        // when when somebody has screwed with setTimeout but no I.E. maddness\n        return cachedSetTimeout(fun, 0);\n    } catch(e){\n        try {\n            // When we are in I.E. but the script has been evaled so I.E. doesn't trust the global object when called normally\n            return cachedSetTimeout.call(null, fun, 0);\n        } catch(e){\n            // same as above but when it's a version of I.E. that must have the global object for 'this', hopfully our context correct otherwise it will throw a global error\n            return cachedSetTimeout.call(this, fun, 0);\n        }\n    }\n\n\n}\nfunction runClearTimeout(marker) {\n    if (cachedClearTimeout === clearTimeout) {\n        //normal enviroments in sane situations\n        return clearTimeout(marker);\n    }\n    // if clearTimeout wasn't available but was latter defined\n    if ((cachedClearTimeout === defaultClearTimeout || !cachedClearTimeout) && clearTimeout) {\n        cachedClearTimeout = clearTimeout;\n        return clearTimeout(marker);\n    }\n    try {\n        // when when somebody has screwed with setTimeout but no I.E. maddness\n        return cachedClearTimeout(marker);\n    } catch (e){\n        try {\n            // When we are in I.E. but the script has been evaled so I.E. doesn't  trust the global object when called normally\n            return cachedClearTimeout.call(null, marker);\n        } catch (e){\n            // same as above but when it's a version of I.E. that must have the global object for 'this', hopfully our context correct otherwise it will throw a global error.\n            // Some versions of I.E. have different rules for clearTimeout vs setTimeout\n            return cachedClearTimeout.call(this, marker);\n        }\n    }\n\n\n\n}\nvar queue = [];\nvar draining = false;\nvar currentQueue;\nvar queueIndex = -1;\n\nfunction cleanUpNextTick() {\n    if (!draining || !currentQueue) {\n        return;\n    }\n    draining = false;\n    if (currentQueue.length) {\n        queue = currentQueue.concat(queue);\n    } else {\n        queueIndex = -1;\n    }\n    if (queue.length) {\n        drainQueue();\n    }\n}\n\nfunction drainQueue() {\n    if (draining) {\n        return;\n    }\n    var timeout = runTimeout(cleanUpNextTick);\n    draining = true;\n\n    var len = queue.length;\n    while(len) {\n        currentQueue = queue;\n        queue = [];\n        while (++queueIndex < len) {\n            if (currentQueue) {\n                currentQueue[queueIndex].run();\n            }\n        }\n        queueIndex = -1;\n        len = queue.length;\n    }\n    currentQueue = null;\n    draining = false;\n    runClearTimeout(timeout);\n}\n\nprocess.nextTick = function (fun) {\n    var args = new Array(arguments.length - 1);\n    if (arguments.length > 1) {\n        for (var i = 1; i < arguments.length; i++) {\n            args[i - 1] = arguments[i];\n        }\n    }\n    queue.push(new Item(fun, args));\n    if (queue.length === 1 && !draining) {\n        runTimeout(drainQueue);\n    }\n};\n\n// v8 likes predictible objects\nfunction Item(fun, array) {\n    this.fun = fun;\n    this.array = array;\n}\nItem.prototype.run = function () {\n    this.fun.apply(null, this.array);\n};\nprocess.title = 'browser';\nprocess.browser = true;\nprocess.env = {};\nprocess.argv = [];\nprocess.version = ''; // empty string to avoid regexp issues\nprocess.versions = {};\n\nfunction noop() {}\n\nprocess.on = noop;\nprocess.addListener = noop;\nprocess.once = noop;\nprocess.off = noop;\nprocess.removeListener = noop;\nprocess.removeAllListeners = noop;\nprocess.emit = noop;\nprocess.prependListener = noop;\nprocess.prependOnceListener = noop;\n\nprocess.listeners = function (name) { return [] }\n\nprocess.binding = function (name) {\n    throw new Error('process.binding is not supported');\n};\n\nprocess.cwd = function () { return '/' };\nprocess.chdir = function (dir) {\n    throw new Error('process.chdir is not supported');\n};\nprocess.umask = function() { return 0; };\n\n\n//# sourceURL=webpack:///./node_modules/process/browser.js?");

/***/ }),

/***/ "./src/Main.elm":
/*!**********************!*\
  !*** ./src/Main.elm ***!
  \**********************/
/*! no static exports found */
/***/ (function(module, exports) {

eval("throw new Error(\"Module build failed (from ./node_modules/elm-webpack-loader/index.js):\\nError: Compiler process exited with error Compilation failed\\nDependencies loaded from local cache.\\nVerifying dependencies...\\rBuilding dependencies (1/7)\\rBuilding dependencies (2/7)\\rBuilding dependencies (3/7)\\rBuilding dependencies (4/7)\\rBuilding dependencies (5/7)\\rBuilding dependencies (6/7)\\rBuilding dependencies (7/7)\\rDependencies ready!                \\n\\r[================                                  ] - 1 / 3\\r[=================================                 ] - 2 / 3\\r[==================================================] - 3 / 3-- NAMING ERROR --- /home/laserpants/code/update-deep/examples/blog/src/Main.elm\\n\\nI cannot find a `UrlReques` type:\\n\\n13|   UrlRequest UrlReques\\n                 ^^^^^^^^^\\nThese names seem close though:\\n\\n    UrlRequest\\n    Url.Url\\n    Result\\n    Url\\n\\nHint: Read <https://elm-lang.org/0.19.0/imports> to see how `import`\\ndeclarations work in Elm.\\n\\r                                                                     \\rDetected errors in 1 module.\\n\\n    at ChildProcess.<anonymous> (/home/laserpants/code/update-deep/examples/blog/node_modules/node-elm-compiler/dist/index.js:131:35)\\n    at emitTwo (events.js:126:13)\\n    at ChildProcess.emit (events.js:214:7)\\n    at maybeClose (internal/child_process.js:915:16)\\n    at Socket.stream.socket.on (internal/child_process.js:336:11)\\n    at emitOne (events.js:116:13)\\n    at Socket.emit (events.js:211:7)\\n    at Pipe._handle.close [as _onclose] (net.js:561:12)\");\n\n//# sourceURL=webpack:///./src/Main.elm?");

/***/ })

/******/ });