Rule = require('models/rule_model').Rule

class exports.RulesTree extends Backbone.Collection

  model: Rule
  url: -> '/rules/inuse'
