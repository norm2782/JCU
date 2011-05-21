ProofTreeNode = require('models/proof_tree_node_model').ProofTreeNode

class exports.ProofTree extends Backbone.Model
  # Available attributes:
  # treeRoot :: RuleTreeNode

  initialize: =>
    @set({treeRoot: new ProofTreeNode()})

  treeRoot: =>
    @get('treeRoot')

  isValid: =>
    @treeRoot().isValid()

  setProofResult: (data) =>
    @treeRoot().setProofResult(data)
