var xhook = require('./js/xhook.min.js');

var users =
[
  {
    id: 1,
    name: 'Mr. Test',
    password: 'test',
    email: 'test@test.com',
    login: 'test'
  }
];

var posts =
[
  {
    id: 1,
    title: '1',
    body: 'hello',
    comments: []
  }
];

var postsId = 1;
var delay = 300;

xhook.before(function(request, callback) {

  if (request.url.endsWith('auth/register') && 'POST' === request.method) {
    setTimeout(function() {
      callback({
        status: 200,
        data: JSON.stringify({ status: 'success' }),
        headers: { 'Content-Type': 'application/json' }
      });
    }, delay);
  }
  else if (request.url.endsWith('auth/login') && 'POST' === request.method) {
    setTimeout(function() {
      var params = JSON.parse(request.body);
      var filtered = users.filter(function(user) {
        return user.login === params.login && user.password === params.password;
      });
      if (filtered.length > 0) {
        var response = filtered[0];
        response.token = 'fake-jwt-token';
        callback({
          status: 200,
          data: JSON.stringify({ user: response }),
          headers: { 'Content-Type': 'application/json' }
        });
      } else {
        console.log('401 Unauthorized');
        callback({
          status: 401,
          data: JSON.stringify({ error: 'Unauthorized' }),
          headers: { 'Content-Type': 'application/json' }
        });
      }
    }, delay);
  } else if (request.url.endsWith('posts')) {
    if ('GET' === request.method) {
      setTimeout(function() {
        callback({
          status: 200,
          data: JSON.stringify({ posts: posts.slice().reverse() }),
          headers: { 'Content-Type': 'application/json' }
        });
      }, delay);
    } else if ('POST' === request.method) {
      setTimeout(function() {
        var post = JSON.parse(request.body);
        post.id = ++postsId;
        post.comments = [];
        posts.push(post);
        callback({
          status: 200,
          data: JSON.stringify({ post: post }),
          headers: { 'Content-Type': 'application/json' }
        });
        //console.log(posts);
      }, delay);
    }
  } else if (/posts\/\d+$/.test(request.url) && 'GET' === request.method) {
    setTimeout(function() {
      var id = request.url.match(/posts\/(\d+)$/)[1];
      var filtered = posts.filter(function(post) { return post.id == id; });
      if (filtered.length > 0) {
        var response = filtered[0];
        callback({
          status: 200,
          data: JSON.stringify({ post: response }),
          headers: { 'Content-Type': 'application/json' }
        });
      } else {
        console.log('Not Found');
        callback({
          status: 404,
          data: JSON.stringify({ error: 'Not Found' }),
          headers: { 'Content-Type': 'application/json' }
        });
      }
    }, delay);
  } else if (/posts\/d+\/comments\/new$/.test(request.url) && 'POST' === request.method) {

    callback();

  } else {

    callback();

  }

});
