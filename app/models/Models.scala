package models

import scala.collection.immutable.TreeSet

// Requirement specified case classes.  I kept the signatures, but adapted more set use with ordering since the problem
// seemed to be helped by that.
case class Rate(rateCode: String, rateGroup: String)
case class CabinPrice(cabinCode: String, rateCode: String, price: BigDecimal)
case class BestGroupPrice(cabinCode: String, rateCode: String, price: BigDecimal, rateGroup: String)

case class Promotion(code: String, notCombinableWith: Seq[String]) extends Ordered[Promotion] {
    private val notCombinable : TreeSet[String] = TreeSet.from(notCombinableWith)
    val orderable : String = notCombinable.mkString(",")

    def combinable(other: Promotion) : Boolean = code != other.code && !notCombinable.contains(other.code) && !other.notCombinable.contains(code)
    def combinable(others: Set[Promotion]) : Boolean = others.forall(o => combinable(o)) && others.headOption.forall(o => o.combinable(others.tail))

    override def compare(that: Promotion): Int =
        code compare that.code match {
            case 0 => orderable compare that.orderable
            case other => other
        }
}

case class PromotionCombo(promotionCodes: Seq[String]) extends Ordered[PromotionCombo] {
    val orderable : String = promotionCodes.mkString(",")
    override def compare(that: PromotionCombo): Int = orderable compare that.orderable
}

// my helper case class to handle aggregation and transformations
case class PromotionCombination(promo: Promotion, combinesWith: TreeSet[Promotion] = TreeSet.empty[Promotion]) extends Ordered[PromotionCombination] {
    val orderable : String = combinesWith.mkString(",")

    def assess(another: Promotion) : PromotionCombination = if (promo.combinable(another)) PromotionCombination(promo, combinesWith + another) else this
    def combine(another: PromotionCombination) : Seq[PromotionCombination] =
        if (promo.code == another.promo.code) {
            Seq(PromotionCombination(promo, combinesWith.concat(another.combinesWith)))
        } else {
            Seq(another, this)
        }

    override def compare(that: PromotionCombination): Int =
        promo compare that.promo match {
            case 0 => orderable compare orderable
            case other => other
        }

    private def variations(elements: Int = combinesWith.size): Set[Set[Promotion]]=
        elements match {
            case 0 => Set.empty[Set[Promotion]]
            case x => variations(x-1) ++ combinesWith.toSeq.combinations(x).map(_.toSet)
        }

    def toCombos : Set[Set[String]] =
        variations().filter(v => promo.combinable(v)).map(cv => Set(promo.code) ++ cv.map(_.code))
}

