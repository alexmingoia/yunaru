"use strict";

const AWS = require('aws-sdk');
const mailparser = require('mailparser');
const fetch = require('node-fetch');

const defaultConfig = {
  webhookSecret: process.env.WEBHOOK_SECRET,
  webhookUrl: process.env.WEBHOOK_URL
};

exports.parseEvent = function(data) {
  // Validate characteristics of a SES event record.
  if (!data.event ||
      !data.event.hasOwnProperty('Records') ||
      data.event.Records.length !== 1 ||
      !data.event.Records[0].hasOwnProperty('eventSource') ||
      data.event.Records[0].eventSource !== 'aws:ses' ||
      data.event.Records[0].eventVersion !== '1.0') {
    data.log({
      message: "parseEvent() received invalid SES message:",
      level: "error", event: JSON.stringify(data.event)
    });
    return Promise.reject(new Error('Error: Received invalid SES message.'));
  }

  data.email = data.event.Records[0].ses.mail;
  data.recipients = data.event.Records[0].ses.receipt.recipients;
  return Promise.resolve(data);
};

exports.fetchMessage = function(data) {
  // Copying email object to ensure read permission
  data.log({
    level: "info",
    message: "Fetching email at s3://" + data.config.emailBucket + '/' +
      data.config.emailKeyPrefix + data.email.messageId
  });
  return new Promise(function(resolve, reject) {
    // Load the raw email from S3
    data.s3.getObject({
      Bucket: data.config.emailBucket,
      Key: data.config.emailKeyPrefix + data.email.messageId
    }, function(err, result) {
      if (err) {
        data.log({
          level: "error",
          message: "getObject() returned error:",
          error: err,
          stack: err.stack
        });
        return reject(
          new Error("Error: Failed to load message body from S3."));
      }
      data.emailData = result.Body.toString();
      return resolve(data);
    });
  });
};

exports.processMessage = function(data) {
  return mailparser.simpleParser(data.emailData).then((parsed) => {
    const from = parsed['reply-to'] || parsed.from;
    return {
      fromEmail: from.value[0] ? from.value[0].address : '',
      fromName: from.value[0] ? from.value[0].name : '',
      recipientId: parsed.to.value[0] ? parsed.to.value[0].address.split('@').shift() : '',
      subject: parsed.subject,
      body: parsed.html || parsed.textAsHtml || parsed.text
    };
  });
};

exports.sendWebhook = function(data) {
  const params = new URLSearchParams();
  params.append('secret', config.webhookSecret);
  params.append('from_email', data.fromEmail);
  params.append('from_name', data.fromName);
  params.append('recipient_id', data.recipientId);
  params.append('subject', data.subject);
  params.append('body', data.body);
  return fetch(config.webhookUrl, {
    method: 'POST',
    body: params
  })
};

exports.handler = function(event, context, callback, overrides) {
  var steps = overrides && overrides.steps ? overrides.steps :
    [
      exports.parseEvent,
      exports.fetchMessage,
      exports.processMessage,
      exports.sendWebhook
    ];
  var data = {
    event: event,
    callback: callback,
    context: context,
    config: overrides && overrides.config ? overrides.config : defaultConfig,
    log: overrides && overrides.log ? overrides.log : console.log,
    ses: overrides && overrides.ses ? overrides.ses : new AWS.SES(),
    s3: overrides && overrides.s3 ?
      overrides.s3 : new AWS.S3({signatureVersion: 'v4'})
  };
  Promise.series(steps, data)
    .then(function(data) {
      data.log({
        level: "info",
        message: "Process finished successfully."
      });
      return data.callback();
    })
    .catch(function(err) {
      data.log({
        level: "error",
        message: "Step returned error: " + err.message,
        error: err,
        stack: err.stack
      });
      return data.callback(new Error("Error: Step returned error."));
    });
};

Promise.series = function(promises, initValue) {
  return promises.reduce(function(chain, promise) {
    if (typeof promise !== 'function') {
      return Promise.reject(new Error("Error: Invalid promise item: " +
        promise));
    }
    return chain.then(promise);
  }, Promise.resolve(initValue));
};
