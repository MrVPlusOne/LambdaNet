// All: 15 JSNice: 9 LambdaNet: 15

import {Url} from "url";
import * as url from "url";

const SpokesDashboardHost = "apps.bespoken.io/dashboard";
const SpokesPipeDomain = "bespoken.link";

function mangleNoPath(sourceID: string, secretKey: string): string {
    let newUrl = "https://";
    newUrl += SpokesDashboardHost;
    newUrl += "?id=" + sourceID;
    newUrl += "&key=" + secretKey;
    return newUrl;
}

function manglePipeToPath(sourceID: string, secretKey?: string) {
    const secureParam = secretKey ? "?bespoken-key=" + secretKey : "";
    return `https://${sourceID}.${SpokesPipeDomain}${secureParam}`;
}

function mangleJustPath(path: string, sourceID: string, secretKey: string): string {
    let newUrl = "https://";
    newUrl += SpokesDashboardHost;
    newUrl += path;
    if (path.indexOf("?") !== -1) {
        newUrl += "&";
    } else {
        newUrl += "?";
    }
    newUrl += "id=" + sourceID;
    newUrl += "&key=" + secretKey;

    return newUrl;
}

function mangle(urlString: string, sourceID: string, secretKey: string): string {
    let urlValue: Url = url.parse(urlString, true, false);
    return mangleJustPath(urlValue.path, sourceID, secretKey);
}


async function createExternalResources(secretKey?: string, sourceID?: string): Promise<any> {
    const sourceData = await this.createSource(secretKey, sourceID);
    const pipe = await this.createSpokesPipe(sourceData.id, sourceData.key);
    return pipe;
}

