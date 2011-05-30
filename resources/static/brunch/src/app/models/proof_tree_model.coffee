ProofTreeNode = require('models/proof_tree_node_model').ProofTreeNode

class exports.ProofTree extends Backbone.Model
  # Available attributes:
  # treeRoot :: RuleTreeNode

  initialize: =>
    @set({treeRoot: new ProofTreeNode({treeLvl: 0, treeLbl: "0", disabled: false})})

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

  setUnified: (tr) =>
    console.log tr
    @treeRoot().setUnified(tr)
