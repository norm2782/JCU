ProofTreeNode = require('models/proof_tree_node').ProofTreeNode

class exports.ProofTree extends Backbone.Model
  # Available attributes:
  # root :: RuleTreeNode
  url: -> '/rules/inuse'

  initialize: ->
    @set {root: new ProofTreeNode()}
    @fetch()

  allValid: ->
    @get('root').isValid()
