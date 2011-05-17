RuleTreeNode = require('models/rule_tree_node').RuleTreeNode

class exports.RuleTree extends Backbone.Model
  # Available attributes:
  # root :: RuleTreeNode
  url: -> '/rules/inuse'

  initialize: ->
    @set {root: new RuleTreeNode()}

  allValid: ->
    @get('root').isValid()
