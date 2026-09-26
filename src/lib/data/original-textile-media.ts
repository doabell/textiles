import mediaData from "./original-textile-media.json";

export type TextileAnnotation = {
    number: number;
    text: string;
    left: number;
    top: number;
    width: number;
    height: number;
};

export type TextileGalleryItem = {
    image: string;
    originalUrl: string;
    title: string;
    caption: string;
    creator: string;
    date: string;
    type: string;
    credit: string;
    catalogueUrl: string;
};

export type TextileStory = Omit<TextileGalleryItem, "type"> & {
    width: number;
    height: number;
    annotations: TextileAnnotation[];
};

export type TextileMedia = {
    sourceUrl: string;
    stories: TextileStory[];
    gallery: TextileGalleryItem[];
};

export const originalTextileMedia = mediaData as unknown as Record<string, TextileMedia>;
