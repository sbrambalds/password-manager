'use strict'

const webpack                 = require('webpack');
const fs                      = require('fs');
const path                    = require('path');
const child_process           = require('child_process');
const EmitFilePlugin          = require('emit-file-webpack-plugin');
const HtmlWebpackPlugin       = require('html-webpack-plugin');
const HtmlInlineScriptPlugin  = require('html-inline-script-webpack-plugin');


function resource(resourceName) {
  return path.resolve(__dirname, 'src/main/resources', resourceName);
}

function purescript(scriptName) {
  return path.resolve(__dirname, 'src/main/purescript', scriptName);
}

function encodeImageToBase64(filePath) {
  const file = fs.readFileSync(filePath);
  return `data:image/png;base64,${file.toString("base64")}`;
}

function git(command) {
  return child_process.execSync(`git ${command}`, { encoding: 'utf8' }).trim();
}

function injectManifest() {
  const manifest = JSON.parse(fs.readFileSync("clipperz.webmanifest", 'utf8'));

  manifest.icons = manifest.icons.map(icon => ({
    ...icon,
    src: encodeImageToBase64(resource(icon.src))
  }));
   
  manifest.screenshots = manifest.screenshots.map(screenshot => ({
    ...screenshot,
    src: encodeImageToBase64(resource(screenshot.src))
  }));

  return JSON.stringify(manifest)
}

module.exports = (env) => {
  return {
    mode: env.production ? 'production' : 'development',
    watchOptions: {
      aggregateTimeout: 200,
      poll: 500,
      ignored: /node_modules/
    },
    entry: {
      app:                purescript('_app_main.js'),
      passwordGenerator:  purescript('_passwordGenerator_main.js'),
      share:              purescript('_share_main.js'),
      redeem: 			      purescript('_redeem_main.js'),
    },
    output: {
      filename: env.production ? '[name]-bundle.js' : '[name]-bundle.[fullhash].js',
      path: path.resolve(__dirname, 'target', "output.webpack"),
      clean: true,
    },
    optimization: {
      minimize: true,
    },
    plugins: [
      new webpack.ProvidePlugin({
        process: 'process/browser',
      }),
      // environmental variables definition
      new webpack.EnvironmentPlugin({
        CURRENT_COMMIT:       env.production ? git('rev-parse HEAD')  : "development",
        APP_URL:	            env.production ? '/app' 				        : '/api/static/index.html',
        SHARE_URL:            env.production ? '/share/#' 			      : '/api/static/share_index.html#',
        REDEEM_URL:           env.production ? '/share/redeem/' 	    : '/api/static/redeem_index.html#',
        DONATION_IFRAME_URL:  env.production ? '/donations/app/'      : 'http://localhost:9090/iframe.html',
      }),
      // app html package configuration
      new HtmlWebpackPlugin({
        template: '/src/main/html/index.html',
        filename: 'index.html',
        scriptLoading: 'module',
        chunks: ["app"],
        minify: true,
        manifest_href: env.production ? "/versions/epsilon/manifest" : "/api/static/clipperz.webmanifest",
        icon_href: encodeImageToBase64(resource("icons/favicon.png"))
      }),
      new HtmlInlineScriptPlugin({
        htmlMatchPattern: [/index.html$/],
        scriptMatchPattern: [/app-bundle(.[a-zA-Z0-9]+)?.js/],
      }),
      new EmitFilePlugin({
        filename: `clipperz.webmanifest`,
        content: injectManifest("clipperz.webmanifest")
      }),
      ...(env.production ? [] : [
        // password generator html package configuration
        new HtmlWebpackPlugin({
          template: '/src/main/html/passwordGenerator_index.html',
          filename: 'passwordGenerator_index.html',
          scriptLoading: 'module',
          chunks: ["passwordGenerator"],
          minify: true,
        }),
        new HtmlInlineScriptPlugin({
          htmlMatchPattern: [/passwordGenerator_index.html$/],
          scriptMatchPattern: [/passwordGenerator-bundle(.[a-zA-Z0-9]+)?.js/],
        }),
        // share html package configuration
        new HtmlWebpackPlugin({
          template: '/src/main/html/share_index.html',
          filename: 'share_index.html',
          scriptLoading: 'module',
          chunks: ["share"],
          minify: true,
        }),
        new HtmlInlineScriptPlugin({
          htmlMatchPattern: [/share_index.html$/],
          scriptMatchPattern: [/share-bundle(.[a-zA-Z0-9]+)?.js/],
        }),
        // redeem html package configuration
        new HtmlWebpackPlugin({
          template: '/src/main/html/redeem_index.html',
          filename: 'redeem_index.html',
          scriptLoading: 'module',
          chunks: ["redeem"],
          minify: true,
        }),
        new HtmlInlineScriptPlugin({
          htmlMatchPattern: [/redeem_index.html$/],
          scriptMatchPattern: [/redeem-bundle(.[a-zA-Z0-9]+)?.js/],
        }),
      ]),
    ],
    module: {
      rules: [
        {
          test: /\.m?js/,
          resolve: { fullySpecified: false },
        },
        {
          test: /\.s[ac]ss$/i,
          use: [
            // Creates `style` nodes from JS strings
            "style-loader",
            // Translates CSS into CommonJS
            "css-loader",
            // Compiles Sass to CSS
            "sass-loader",
          ],
        },
      ]
    }
  }
};