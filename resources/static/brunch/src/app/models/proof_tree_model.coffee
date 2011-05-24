ProofTreeNode = require('models/proof_tree_node_model').ProofTreeNode

class exports.ProofTree extends Backbone.Model
  # Available attributes:
  # treeRoot :: RuleTreeNode

  initialize: =>
    @set({treeRoot: new ProofTreeNode({treeLvl: 0, treeLbl: "0"})})

  reset: =>
    @treeRoot().setTerm ""
    @treeRoot().reset()

  treeRoot: =>
    @get('treeRoot')

  isValid: =>
    @treeRoot().isValid()

  isProved: =>
    @treeRoot().isProved()

  setProofResult: (data) =>
    @treeRoot().setProofResult data
