var sakai = sakai || {};

sakai.config = sakai.config || {};

// Insert custom configuration here

sakai.config.env = "qa";

sakai.config.Navigation = [
    {
        "url" : "/dev/my_sakai.html",
        "label" : "MY_SAKAI"
    }
];

// This is our custom CAS log in information. Please note, we're not actually using the external object yet 
// For production and QA, sent internal to false, for dev set it to true.

sakai.config.Authentication = {
    "internal": true,
    "external": [
        {
          label: "LOGIN_TITLE",
          login_btn: "LOGIN_BTN",
          url: "/system/sling/cas/login?resource=/dev/index.html",
          description: "CAS_NOTE"
        }
    ]
};
