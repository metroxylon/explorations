//tealium universal tag - utag.sync ut4.0.201801191304, Copyright 2018 Tealium.com Inc. All Rights Reserved.
var isMobile={Android:function(){return navigator.userAgent.match(/Android/i);},BlackBerry:function(){return navigator.userAgent.match(/BlackBerry/i);},iOS:function(){return navigator.userAgent.match(/iPhone|iPad|iPod/i);},Opera:function(){return navigator.userAgent.match(/Opera Mini/i);},Windows:function(){return navigator.userAgent.match(/IEMobile/i)||navigator.userAgent.match(/WPDesktop/i);},any:function(){return(isMobile.Android()||isMobile.BlackBerry()||isMobile.iOS()||isMobile.Opera()||isMobile.Windows());}};if(!isMobile.any()){document.write('<script type="text/javascript" src="//cdn.optimizely.com/js/7779511712.js"></scr'+'ipt>');}