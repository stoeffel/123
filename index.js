var assets = require('./assets/*.png');
// var sounds = require('./assets/*.wav');
// var less = require('./src/main.less');
var { Elm } = require('./src/Main.elm');

(function(){
var setViewportHeight = (function(){
    function debounced(){
    document.documentElement.style.height = window.innerHeight + "px";
    if (document.body.scrollTop !== 0) {
        window.scrollTo(0, 0);
    }
    }
    var cancelable = null;

    return function(){
    cancelable && clearTimeout(cancelable);
    cancelable = setTimeout(debounced, 100);
    };
})();

// ipad safari
if(/iPad/.test(navigator.platform) && /Safari/i.test(navigator.userAgent)){  
    window.addEventListener("resize", setViewportHeight, false);
    window.addEventListener("scroll", setViewportHeight, false);
    window.addEventListener("orientationchange", setViewportHeight, false);
    setViewportHeight();
}
})();

function main () {
  Elm.Main.init({flags: { assets: assets }});
}

// HMR setup. For more info see: https://parceljs.org/hmr.html
if (module.hot) {
  module.hot.accept(function () {
    console.log('Reloaded, running main again');
    document.body.innerHTML = '';
    main();
  });
}

console.log('Starting app');

main();
