class RulesList extends Backbone.Collection

  model: Rule
  url: -> 'http://localhost:8000/rules/stored'
