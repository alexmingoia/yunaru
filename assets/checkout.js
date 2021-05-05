function submitFormAndRedirectToCheckout(event) {
  event.preventDefault();

  var form = event.currentTarget;

  if (form.getAttribute('method') === 'GET') {
    waitForScripts(redirectToCheckout);
  } else {
    waitForScripts(function() {
      var formUrl = form.getAttribute('action');
      var formData = new FormData(form);
      doXHR(formUrl, formData, function(err, _, status) {
        if (err) {
          console.log(err);
          return window.location.href = '/users/new';
        }
        redirectToCheckout();
      });
    });
  }
}

function waitForScripts(cb) {
  var checkStripeLoaded = setInterval(function() {
    if (typeof Stripe === 'function' && typeof doXHR === 'function') {
      clearInterval(checkStripeLoaded);
      cb();
    }
  }, 500);
}

function redirectToCheckout() {
  var stripe = Stripe(stripeKey);

  doXHR('/payments/stripe/checkout-sessions', null, function(err, id, status) {
    if (err) {
      console.log(err);
      return window.location.href = '/users/new';
    }
    stripe.redirectToCheckout({ sessionId: id });
  });
}
