class RulesTree extends Backbone.Collection

  model: Rule
  url: -> '/rules/inuse'
