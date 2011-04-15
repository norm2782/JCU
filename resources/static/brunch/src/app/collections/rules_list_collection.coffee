class RulesList extends Backbone.Collection

  model: Rule
  url: -> '/rules/stored'
