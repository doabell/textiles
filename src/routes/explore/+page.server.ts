import { getExplorerOptions, getTradeRecords } from "$lib/server/trade";
import type { PageServerLoad } from "./$types";

export const load: PageServerLoad = () => {
    const records = getTradeRecords();

    return {
        records,
        options: getExplorerOptions(records),
    };
};
