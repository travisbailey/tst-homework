package services

import models.{BestGroupPrice, CabinPrice, Promotion, PromotionCombination, PromotionCombo, Rate}
import utils.Extensions._

trait RateService {
    def getBestGroupPrices(rates: Seq[Rate], prices: Seq[CabinPrice]): Seq[BestGroupPrice]
    def allCombinablePromotions(allPromotions: Seq[Promotion]): Seq[PromotionCombo]
    def combinablePromotions(promotionCode: String, allPromotions: Seq[Promotion]): Seq[PromotionCombo]
}

class DefaultRateService extends RateService {
    def getBestGroupPrices(rates: Seq[Rate], prices: Seq[CabinPrice]) : Seq[BestGroupPrice] = {
        val ratesByCode = rates.map(r => r.rateCode -> r).toMap
        prices.flatMap(p => ratesByCode.get(p.rateCode)
            .map(r => BestGroupPrice(p.cabinCode, r.rateCode, p.price, r.rateGroup)))
            .groupBy(bgr => (bgr.cabinCode, bgr.rateGroup))
            .flatMap(g => g._2.sortBy(_.price).headOption)
            .toSeq
    }

    def allCombinablePromotions(allPromotions: Seq[Promotion]) : Seq[PromotionCombo] =
        process(allPromotions)

    def combinablePromotions(promotionCode: String, allPromotions: Seq[Promotion]) : Seq[PromotionCombo] =
        // given the expense of these operations, if larger sets are introduced, it would make sense to attempt
        // to filter allPromotions down to those that are relevant to the supplied promotion
        process(allPromotions)
            .filter(_.promotionCodes.contains(promotionCode))
            .map(pc => PromotionCombo(Seq(promotionCode) ++ pc.promotionCodes.filterNot(_ == promotionCode)))

    private def process(promotions: Seq[Promotion]) : Seq[PromotionCombo] = {
        // choosing to apply a cross product on the promotions to generate all possible relations and then filter.
        // this is naturally creates a O(n^2) balloon, but not sure I can save a lot here
        val reduced = promotions.cross(promotions)
            // initialize a holding class that is really an inversion of the supplied Promotion type
            .map(p => PromotionCombination(p._1).assess(p._2)).toList.sorted
            // collection is sorted to allow for a folding aggregation of all possible combinable promotions for each promotion
            .foldLeft[Seq[PromotionCombination]](Nil) { (acc, combo) =>
                acc match {
                    case Nil => Seq(combo)
                    case lastCombo :: tail => lastCombo.combine(combo) ++ acc.tail
                }
            }
            // this converts to a to sets
            .flatMap(_.toCombos)
            .toSet

        // do some set filtering and ordering merely because requirements seemed to imply the requirement
        reduced.filterNot(cv => (reduced - cv).exists(another => cv.subsetOf(another)))
            .map(ns => PromotionCombo(ns.toSeq.sorted))
            .toSeq
    }
}

// I'm creating a simple object based way for this since it is for testing exercises... I'd likely leverage actual
// class instances in a real application
object RateService extends DefaultRateService
