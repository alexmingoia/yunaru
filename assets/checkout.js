function submitFormAndRedirectToCheckout(event) {
  event.preventDefault();

  var form = event.currentTarget;
  var formData = new FormData(form);

  if (form.getAttribute('method') === 'GET') {
    waitForScripts(function() {
      redirectToCheckout(formData);
    });
  } else {
    waitForScripts(function() {
      var formUrl = form.getAttribute('action');
      doXHR(formUrl, formData, function(err, _, status) {
        if (err) {
          console.log(err);
          return form.submit();
        }
        redirectToCheckout(formData);
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

function redirectToCheckout(formData) {
  var stripe = Stripe(stripeKey);

  doXHR('/payments/stripe/checkout-sessions', formData, function(err, id, status) {
    if (err) {
      console.log(err);
      return window.location.href = '/users/new';
    }
    stripe.redirectToCheckout({ sessionId: id });
  });
}
