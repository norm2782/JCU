RuleTreeNode = require('models/rule_tree_node').RuleTreeNode

class exports.RuleTree extends Backbone.Model
  # Available attributes:
  # root :: RuleTreeNode

  initialize: ->
    @set {root: new RuleTreeNode()}
