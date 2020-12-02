package services

import models.{BestGroupPrice, CabinPrice, Promotion, PromotionCombo, Rate}
import org.scalatestplus.play.PlaySpec

class RateServiceSpec extends PlaySpec {

    "Rate Service" should {

        "return expected BestGroupPrices from specific input of rates and prices" in {
            val rates = Seq(
                Rate("M1", "Military"),
                Rate("M2", "Military"),
                Rate("S1", "Senior"),
                Rate("S2", "Senior")
            )

            val cabinPrices = Seq(
                CabinPrice("CA", "M1", 200.00),
                CabinPrice("CA", "M2", 250.00),
                CabinPrice("CA", "S1", 225.00),
                CabinPrice("CA", "S2", 260.00),
                CabinPrice("CB", "M1", 230.00),
                CabinPrice("CB", "M2", 260.00),
                CabinPrice("CB", "S1", 245.00),
                CabinPrice("CB", "S2", 270.00)
            )

            val expectations = Seq(
                BestGroupPrice("CA", "M1", 200.00, "Military"),
                BestGroupPrice("CA", "S1", 225.00, "Senior"),
                BestGroupPrice("CB", "M1", 230.00, "Military"),
                BestGroupPrice("CB", "S1", 245.00, "Senior")
            )

            val result = RateService.getBestGroupPrices(rates, cabinPrices)

            expectations.foreach( e =>
                result must contain(e)
            )

            result.size mustBe 4
        }

        val promos = Seq(
            Promotion("P1", Seq("P3")), // P1 is not combinable with P3
            Promotion("P2", Seq("P4", "P5")), // P2 is not combinable with P4 and P5
            Promotion("P3", Seq("P1")), // P3 is not combinable with P1
            Promotion("P4", Seq("P2")), // P4 is not combinable with P2
            Promotion("P5", Seq("P2")) // P5 is not combinable with P2
        )

        "return expected collection of PromotionCombos from specific input of Promotions" in {

            val expectations = Seq(
                PromotionCombo(Seq("P1", "P2")),
                PromotionCombo(Seq("P1", "P4", "P5")),
                PromotionCombo(Seq("P2", "P3")),
                PromotionCombo(Seq("P3", "P4", "P5"))
            )

            val result = RateService.allCombinablePromotions(promos)

            expectations.foreach( e =>
                result must contain(e)
            )

            result.size mustBe 4
        }

        "return expected PromotionCombos for promotion 'P1'" in {

            val expectations = Seq(
                PromotionCombo(Seq("P1", "P2")),
                PromotionCombo(Seq("P1", "P4", "P5"))
            )

            val result = RateService.combinablePromotions("P1", promos)

            expectations.foreach( e =>
                result must contain(e)
            )

            result.size mustBe 2
        }

        "return expected PromotionCombos for promotion 'P3'" in {

            val expectations = Seq(
                PromotionCombo(Seq("P3", "P2")),
                PromotionCombo(Seq("P3", "P4", "P5"))
            )

            val result = RateService.combinablePromotions("P3", promos)

            expectations.foreach( e =>
                result must contain(e)
            )

            result.size mustBe 2
        }
    }
}
