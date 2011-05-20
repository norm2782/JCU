class exports.ProofTreeNode extends Backbone.Model
  # Available attributes:
  # term :: Term
  # childTerms :: BackBone.Collection

  initialize: =>
    @set {childTerms: new Backbone.Collection()}

  hasTerm: =>
    @get('term')?

  getChildTerms: =>
    @get('childTerms')

  setChildNo: (childNo) =>
    newChildren = new Array()
    newChildren.push(new ProofTreeNode()) for i in [1..childNo] if childNo > 0
    @getChildTerms().refresh(newChildren)
