Rule = require('models/rule_model').Rule

class exports.RulesList extends Backbone.Collection

  model: Rule
  url: => '/rules/stored'

  parse: (resp) =>
    _.map resp, (x) -> {id: x, rule: x}
