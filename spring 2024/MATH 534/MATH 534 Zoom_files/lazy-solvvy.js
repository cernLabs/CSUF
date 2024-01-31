(function () {

    //LAZY GLOBALS
    var LAZY_WIDGET_ID = 'solvvy-lazy-button';
    var LAZY_WIDGET_STYLES_ID = 'solvvy-lazy-button-styles';


    //ZVA GLOBALS
    var ZVA_INSTALL_SCRIPT = 'https://us01ccistatic.zoom.us/us01cci/web-sdk/chat-client.js';
    var ZVA_ACTIVE_COOKIE = '_zvaforce';

    //AB GLOBALS
    var AB_TEST_VERSION = getCookie("abTestVersion");


    //DOCUMENT INNER HTML
    var LAZY_WIDGET_LOADING_INNER_HTML = `
  <div class="svg-icon widget-icon">
    <div class="solvvy-loading"><div></div><div></div><div></div><div></div></div>
  </div>
  `;
    var LAZY_WIDGET_INNER_HTML = `
  <div class="svg-icon widget-icon">
    <svg xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 16 16">
      <mask height="16" width="14" y="0" x="1" maskUnits="userSpaceOnUse" style="mask-type:alpha" id="mask0_4_176">
        <path fill="#000001" d="M15 6.99999C15 3.10079 11.8118 -0.0538043 7.90011 0.000695695C4.18261 0.0524957 1.05229 3.1831 1.00069 6.9006C0.948592 10.6511 3.8468 13.7364 7.5226 13.984C7.7889 14.0019 8.00051 14.2098 8.00051 14.4766V15.4931C8.00051 15.8314 8.33412 16.0853 8.65322 15.973C12.3483 14.6725 15.0001 11.1399 15 6.99999ZM8.00011 3.49999C8.88411 3.49999 9.60012 4.2163 9.60012 5.1C9.60012 5.9837 8.88411 6.70001 8.00011 6.70001C7.11611 6.70001 6.40011 5.9837 6.40011 5.1C6.40011 4.2163 7.11611 3.49999 8.00011 3.49999ZM10.0421 10.4755C10.0151 10.4809 9.98811 10.4858 9.96111 10.4892C9.90811 10.4963 9.8551 10.4999 9.8001 10.4999H6.20009C5.53709 10.4999 5.00011 9.96279 5.00011 9.29989C5.00011 8.30579 6.20011 7.4999 8.00011 7.4999C9.10911 7.4999 9.9901 7.80579 10.5031 8.27279C10.8231 8.56429 11.0001 8.91809 11.0001 9.29989C11.0001 9.87979 10.5891 10.3637 10.0421 10.4755Z"></path>
      </mask>
      <g mask="url(#mask0_4_176)">
        <rect fill="#FFFFFF" height="16" width="16"></rect>
      </g>
    </svg>
  </div>
  `;

    var LAZY_WIDGET_CSS = `
  .${LAZY_WIDGET_ID} {
    position: fixed;
    z-index: 1900000000;
    bottom: 0px;
    right: 0px;
    width: 60px;
    height: 60px;
    margin: 20px;
    padding: 12px;
    background: #0B5CFF;
    border-radius: 26px;
    box-shadow: 0px 0px 18px 3px rgb(0 0 0 / 35%);
    cursor: pointer;
    border: none;
  }

  .${LAZY_WIDGET_ID}:hover {
    background: #0050F0;
  }

  .${LAZY_WIDGET_ID} .solvvy-loading {
    display: inline-block;
    position: relative;
    width: 36px;
    height: 36px;
  }
  .${LAZY_WIDGET_ID} .solvvy-loading div {
    box-sizing: border-box;
    display: block;
    position: absolute;
    width: 36px;
    height: 36px;
    border: 4px solid #fff;
    border-radius: 50%;
    animation: solvvy-loading 1.2s cubic-bezier(0.5, 0, 0.5, 1) infinite;
    border-color: #fff transparent transparent transparent;
  }
  .${LAZY_WIDGET_ID} .solvvy-loading div:nth-child(1) {
    animation-delay: -0.45s;
  }
  .${LAZY_WIDGET_ID} .solvvy-loading div:nth-child(2) {
    animation-delay: -0.3s;
  }
  .${LAZY_WIDGET_ID} .solvvy-loading div:nth-child(3) {
    animation-delay: -0.15s;
  }
  @keyframes solvvy-loading {
    0% {
      transform: rotate(0deg);
    }
    100% {
      transform: rotate(360deg);
    }
  }
  .slv-loading {
      border: 2px solid #ccc;
      width: 15px;
      height: 15px;
      border-radius: 50%;
      border-top-color: #747487;
      border-left-color:  #747487;
      animation: slv-spin 1s infinite ease-in;
      margin-left: 10px;
    }
    @keyframes slv-spin {
      0% {
          transform: rotate(0deg);
      }
      100% {
          transform: rotate(360deg);
      }
    }
  `;


    function getCookie(name) {
        const cookies = document.cookie.split("; ");
        for (let i = 0; i < cookies.length; i++) {
            const parts = cookies[i].split("=");
            if (parts[0] === name) {
                return parts[1];
            }
        }
        return null;
    }


    //Function to set ZVA session cookie
    function setZVACookie(name, value) {
        const date = new Date();
        date.setTime(date.getTime() + 1 * 24 * 60 * 60 * 1000); // 1 day in milliseconds
        const expires = "; expires=" + date.toUTCString();
        document.cookie = name + "=" + value + expires + ";domain=.zoom.us; path=/";
        document.cookie = name + "=" + value + expires + ";domain=.zoom.com; path=/";
        document.cookie = name + "=" + value + expires + ";domain=.zendesk.com; path=/";
        document.cookie = name + "=" + value + expires + ";domain=.zoomdev.us; path=/";
    }

    /*************************************************************************************************************************************************/


    /************************************************************* LAZY WIDGET FUNCTIONS *************************************************************/
    // Returns Lazy Widget object in document
    function getLazyWidget() {
      return document.getElementById(LAZY_WIDGET_ID);
    }

    // Appends Lazy Widget CSS to Document
    function addLazyWidgetStyles() {
      var style = document.createElement('style');
      style.id = LAZY_WIDGET_STYLES_ID;
      style.appendChild(document.createTextNode(LAZY_WIDGET_CSS));
      document.head.appendChild(style);
    }

    // Appends Lazy Widget to document
    function addLazyWidget() {
      var widget = document.createElement('button');
      widget.id = LAZY_WIDGET_ID;
      widget.classList = LAZY_WIDGET_ID;
      widget.onclick = onLazyWidgetClick;
      widget.innerHTML = LAZY_WIDGET_INNER_HTML;
      widget.setAttribute('aria-label', 'Chat with bot');
      widget.setAttribute('title', 'Chat with bot');
      addLazyWidgetStyles();
      document.body.appendChild(widget);
    }

    // Removes Lazy Widget and related CSS from document
    function removeLazyWidget() {
      var widget = getLazyWidget();
      if (widget) {
          widget.remove();
      }
      var widgetStyles = document.getElementById(LAZY_WIDGET_STYLES_ID);
      if (widgetStyles) {
          widgetStyles.remove();
      }
    }

    //Initializes Lazy Widget. Listens for solvvy_ready to indicate when to remove lazy widget.
    function initLazyWidget() {
            addLazyWidget();
    }
    // Once Lazy Widget is clicked, load correct bot based on AB_TEST_VERSION
    async function onLazyWidgetClick() {
            var widget = getLazyWidget();
            if (widget) { widget.innerHTML = LAZY_WIDGET_LOADING_INNER_HTML; }

            loadZVA();
            waitForZoomSDKAndExecute(function() {
                window.zoomCampaignSdk.hide();
                openZVA();
            });
        }
   


    // Loads ZVA. Sets _zvaforce to bypass lazy load on page refresh
    function loadZVA() {
        var ZVAScript = document.createElement("script");
        ZVAScript.type = "text/javascript";
        ZVAScript.src = ZVA_INSTALL_SCRIPT;
        ZVAScript.setAttribute("data-env", "us01");
        ZVAScript.setAttribute("data-apikey", "AM_FKF55QOG_vdWum455Vg");
        document.body.appendChild(ZVAScript);
        setZVACookie(ZVA_ACTIVE_COOKIE, "true");
    }

    // Returns true if _zvaforce cookie is set
    function hasActiveZvaSession() {
        return document.cookie.indexOf('_zvaforce') > -1;
    }

    // returns true if _abtestzva cookie is set
    function inZvaBucket() {
            return document.cookie.indexOf('_abtestzva') > -1;
        }
    // Waits for zoomCampaignSdk object to exist
    function waitForZoomSDKAndExecute(callback) {
        // Function to check if zoomCampaignSdk exists
        function isZoomSDKLoaded() {
            return typeof window.zoomCampaignSdk !== 'undefined' && typeof window.zoomCampaignSdk.open === 'function' && window.zoomCampaignSdk.campaigns.length > 0 && window.zoomCampaignSdk.campaigns.every(x => x.chat?.chatHandlers?.welcomeSubmitHandler);
        }
        const pollingInterval = 100;
        const maxWaitTime = 10000;
        let elapsedTime = 0;

        const checkZoomSDK = setInterval(() => {
            elapsedTime += pollingInterval;

            if (isZoomSDKLoaded() || elapsedTime >= maxWaitTime) {
                clearInterval(checkZoomSDK);

                if (isZoomSDKLoaded()) {
                    callback();
                } else {
                    console.log('AB999')
                    removeLazyWidget()
                    initLazyWidget()
                }
            }
        }, pollingInterval);
      }

    // Opens ZVA Chatbot window once zoomCampaignSdk is loaded
    function openZVA(){
        setTimeout(function() {
            try {
                window.zoomCampaignSdk.open();
                window.zoomCampaignSdk.show();
                removeLazyWidget();
            } catch(e) {
                //console.info(e)
                openZVA()
            }
        }, 100);
    }



    function run() {// If consumer has interacted with either bot, load that same bot accordingly. Otherwise, load lazy widget.
        if (hasActiveZvaSession()){
            loadZVA();
        } else {
            initLazyWidget()
        }
    }

    try {
        run();
    } catch (e) {
        console.log(e)
    }
})();