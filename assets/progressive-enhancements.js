var supportsQuerySelectors = typeof document.querySelector === 'function' && typeof document.querySelectorAll === 'function';
var supportsToLocaleString = typeof Date.prototype.toLocaleDateString === 'function';
var supportsFormData = typeof window.FormData === 'function';

if (supportsQuerySelectors && supportsToLocaleString && supportsFormData) {
  if (document.readyState === 'interactive' || document.readyState === 'complete') {
    enhance();
  } else {
    window.addEventListener('DOMContentLoaded', enhance);
  }
}

window.addEventListener('pageshow', clearButtonLoading);

function enhance() {
  /* Display timestamps formatted in browser's locale */
  var timeElems = document.getElementsByTagName('time');
  var datetime = '';
  var tsElem = null;
  var locale = undefined;
  var localeOpts = { day: 'numeric', year: 'numeric', month: 'short' };

  for (var i=0; i<timeElems.length; i++) {
    tsElem = timeElems[i];
    if (datetime = tsElem.getAttribute('datetime')) {
      if (tsElem.className.indexOf('compact') === -1 && (Date.now() - (new Date(datetime)).getTime()) > 1209600000) {
        if (tsElem.childNodes.length && tsElem.childNodes[0].tagName === 'A') {
          tsElem = tsElem.childNodes[0];
        }
        tsElem.innerText = (new Date(datetime)).toLocaleDateString(locale, localeOpts);
      }
    }
  }

  window.addEventListener('submit', function(e) {
    var form = e.target;
    var submitBtn = form.querySelector('button[type=submit]');
    if (submitBtn) {
      /* Disable form on submit */
      submitBtn.setAttribute('disabled', 'disabled');
      submitBtn.className = submitBtn.className + ' is-loading';
    }
  });

  /* Reset form buttons on page unload. Otherwise, using back button will show invalid button text. */
  window.addEventListener('unload', clearButtonLoading);
}

function clearButtonLoading() {
  var buttons = document.querySelectorAll('button[type=submit]');
  for (var i = 0, l = buttons.length; i < l; i++) {
    if (buttons[i].hasAttribute('disabled')) {
      buttons[i].removeAttribute('disabled');
      buttons[i].className = buttons[i].className.replace(' is-loading', '');
    }
  }
}

function doXHR(url, data, cb) {
  var xhr = new XMLHttpRequest();
  var finished = false;

  xhr.open('POST', url, true);
  xhr.setRequestHeader('Prefer', 'return-no-content');
  xhr.addEventListener('error', function(err) {
    if (!finished) {
      finished = true;
      cb(err);
    }
  });
  xhr.onreadystatechange = function() {
    if (xhr.readyState === 4 && !finished) {
      var contentType = xhr.getResponseHeader('Content-Type');
      var body = xhr.responseText;
      if (contentType.match('application/json')) {
        body = JSON.parse(body);
      }
      finished = true;
      if (xhr.status >= 200 && xhr.status < 300) {
        cb(null, body);
      } else {
        if (contentType.match('text/plain')) {
          cb(new Error(body), body, xhr.status);
        } else {
          cb(new Error('Response status: ' + xhr.status), body, xhr.status);
        }
      }
    }
  };

  xhr.send(data);
}
