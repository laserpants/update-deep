var dotenv = require('dotenv');
var path = require('path');
var webpack = require('webpack');

module.exports = function() {

  var env = dotenv.config({ path: '.env' }).parsed;

  var keys = env ? Object.keys(env).reduce(function(acc, val) {
    acc['process.env.' + val] = JSON.stringify(env[val]);
    return acc;
  }, {}) : {};

  return {
    mode: 'development',
    entry: './index.js',
    module: {
      rules: [
        {
          test: /\.html$/,
          exclude: /node_modules/,
          loader: 'file-loader?name=[name].[ext]'
        },
        {
          test: /\.elm$/,
          exclude: [ /elm-stuff/, /node_modules/ ],
          loader: 'elm-webpack-loader?verbose=true&warn=true',
          options: {
            debug: true
          }
        }
      ]
    },
    devServer: {
      inline: true,
      stats: 'errors-only',
      historyApiFallback: {
        index: 'index.html'
      }
    },
    plugins: [
      new webpack.DefinePlugin(keys)
    ]
  };

};
