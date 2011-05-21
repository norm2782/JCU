ProofTreeNode = require('models/proof_tree_node_model').ProofTreeNode

class exports.ProofTree extends Backbone.Model
  # Available attributes:
  # treeRoot :: RuleTreeNode

  initialize: =>
    @set({treeRoot: new ProofTreeNode()})

  root: =>
    @get('treeRoot')

  isValid: =>
    @root().isValid()

  setProofResult: (data) =>
    @root().setProofResult(data)
