// Initial Tracking Code
var _paq = _paq || [];
_paq.push(['disableCookies']);
_paq.push(["setDoNotTrack", true]);
_paq.push(['enableLinkTracking']);
_paq.push(['enableHeartBeatTimer']);
_paq.push(['trackPageView']);
(function() {
  var u = 'https://analytics.covid19-r.com/';  
  _paq.push(['setTrackerUrl', u+'matomo.php']);
  _paq.push(['setSiteId', '3']);
  var d = document,
  g = d.createElement('script'),
  s = d.getElementsByTagName('script')[0];
  g.type = 'text/javascript';
  g.async = true;
  g.defer = true;
  g.src = u+'matomo.js';
  s.parentNode.insertBefore(g,s);
})();

// Event Tracking Code
$(document).on('shiny:inputchanged', function(event) {
  if(event.name == "county") {
    _paq.push(['trackEvent', 'selectCounty', event.value]);
  } else if(event.name == "intervention_date") {
    _paq.push(['trackEvent', 'selectDate', event.value]);
  } else if(event.name == "tau") {
    _paq.push(['trackEvent', 'setTau', event.value]);
  } else if(event.name == "plot_ages") {
    _paq.push(['trackEvent', 'setAGES', event.value]);
  }
});