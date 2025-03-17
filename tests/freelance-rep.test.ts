import { describe, expect, it } from "vitest";
import { Cl } from "@stacks/transactions";

const accounts = simnet.getAccounts();
const client = accounts.get("wallet_1")!;
const freelancer = accounts.get("wallet_2")!;

describe("freelance reputation system", () => {
    it("successfully submits a review", () => {
        // Submit a 5-star review
        const submitReview = simnet.callPublicFn(
            "freelance-rep",
            "submit-review",
            [Cl.principal(freelancer), Cl.uint(5)],
            client
        );
        expect(submitReview.result).toBeOk(Cl.bool(true));

        // Check freelancer's rating
        const getRating = simnet.callReadOnlyFn(
            "freelance-rep",
            "get-freelancer-rating",
            [Cl.principal(freelancer)],
            client
        );
        
        const expectedRating = Cl.tuple({
            'total-score': Cl.uint(5),
            'review-count': Cl.uint(1),
            'average-rating': Cl.uint(5)
        });
        
        expect(getRating.result).toBeOk(expectedRating);
    });
    it("prevents invalid ratings", () => {
        const invalidReview = simnet.callPublicFn(
            "freelance-rep",
            "submit-review",
            [Cl.principal(freelancer), Cl.uint(6)],
            client
        );
        expect(invalidReview.result).toBeErr(Cl.uint(101)); // ERR-INVALID-RATING
    });

    it("prevents duplicate reviews", () => {
        // First review
        simnet.callPublicFn(
            "freelance-rep",
            "submit-review",
            [Cl.principal(freelancer), Cl.uint(4)],
            client
        );

        // Attempt duplicate review
        const duplicateReview = simnet.callPublicFn(
            "freelance-rep",
            "submit-review",
            [Cl.principal(freelancer), Cl.uint(5)],
            client
        );
        expect(duplicateReview.result).toBeErr(Cl.uint(102)); // ERR-ALREADY-REVIEWED
    });
});


// new test