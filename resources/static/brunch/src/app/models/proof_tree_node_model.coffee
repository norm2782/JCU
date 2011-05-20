class exports.ProofTreeNode extends Backbone.Model
  # Available attributes:
  # term :: Term
  # childTerms :: BackBone.Collection

  initialize: =>
    @set {childTerms: new Backbone.Collection()}

  hasTerm: =>
    @term?

  term: =>
    @get('term')

  childTerms: =>
    @get('childTerms')

  setChildNo: (childNo) =>
    newChildren = new Array()
    newChildren.push(new ProofTreeNode()) for i in [1..childNo] if childNo > 0
    @childTerms().refresh(newChildren)

  isValid: =>
    if !@hasTerm()
      return false
    @term().validate() && @childTerms().reduce(((c, xs) -> c.isValid() && xs), true)
