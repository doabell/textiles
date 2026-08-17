import { assets } from "$app/paths";

/**
 * Vite's `?url` imports are root-relative in rendered markup. Prefixing them
 * with SvelteKit's route-aware assets path keeps prerendered pages portable
 * when the static build is mounted below `/` or inspected from disk.
 */
export function assetPath(url: string) {
    return url.startsWith("/_app/") ? `${assets}${url}` : url;
}
