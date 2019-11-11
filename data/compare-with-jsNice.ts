/** Taken from AFASSoftware_maquette/src/projector.ts\
 * JSNice [2/3]
 * LambdaNet [1/3]
 * */

let createParentNodePath = (node: Node, rootNode: Element) => {
    let parentNodePath = [];
    while (node !== rootNode) {
        parentNodePath.push(node);
        node = node.parentNode;
    }
    return parentNodePath;
};

/**
 * Taken from akserg_ng2-dnd/src/dnd.utils.ts
 * JSNice: [2/3]
 * LambdaNet [2/3]
 */
function createImage(src: string) {
    let img:HTMLImageElement = new HTMLImageElement();
    img.src = src;
    return img;
}

function callFun(fun: Function) {
    return fun();
}

