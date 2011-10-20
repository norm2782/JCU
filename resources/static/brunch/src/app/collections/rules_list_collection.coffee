Rule = require('models/rule_model').Rule

class exports.RulesList extends Backbone.Collection

  model: Rule
  url: => '/rules/stored'
